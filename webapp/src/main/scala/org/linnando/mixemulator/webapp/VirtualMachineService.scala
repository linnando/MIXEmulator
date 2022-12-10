package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.asm.{MixAssembler, MixDisassembler}
import org.linnando.mixemulator.vm._
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.file._
import org.linnando.mixemulator.vm.io.{BlockDevice, Device, LineDevice}
import rxjs.Subject

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Injectable()
class VirtualMachineService {
  var text: String =
    """START      OUT  HELLO(18)
           JBUS *(18)
           HLT
HELLO      ALF  HELLO
           ALF  , WOR
           ALF  LD
           END START"""
  var mode = "binary"
  var tracking = true

  val stateChange: Subject[Unit] = new Subject()

  private var lines: Vector[String] = Vector.empty

  private var _machine: Option[VirtualMachine] = None

  private var _symbols: Vector[(Option[Short], Option[Int])] = Vector.empty

  private var _addressSymbols: Vector[Int] = Vector.empty

  private var disassembler: MixDisassembler = _

  def assemble(): Future[Unit] = Future {
    lines = text.split("\n").toVector
    val processingModel = mode match {
      case "binary" => binary
      case "decimal" => decimal
    }
    val builder = processingModel.createVirtualMachineBuilder()
      .withDevices(VirtualMachineService.createDevices())
    val assembly =
      if (tracking) MixAssembler.translateTracking(builder, lines)
      else MixAssembler.translateNonTracking(builder, lines)
    _machine = Some(assembly._1)
    _symbols = assembly._2.toVector
    _addressSymbols = _symbols.indices.foldLeft(Vector.fill(VirtualMachine.MEMORY_SIZE)(0)) { (s, index) =>
      val address = _symbols(index)._1
      if (address.isEmpty) s
      else s.updated(address.get, index)
    }
    disassembler = new MixDisassembler(processingModel)
  }

  def go(): Future[Unit] = {
    val devices = VirtualMachineService.createDevices()
    val processingModel = mode match {
      case "binary" => binary
      case "decimal" => decimal
    }
    val eventualMachine =
      if (tracking) processingModel.goTracking(devices, VirtualMachineService.goDevice)
      else processingModel.goNonTracking(devices, VirtualMachineService.goDevice)
    eventualMachine map { machine =>
      _machine = Some(machine)
      _symbols = Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None))
      _addressSymbols = Vector.range(0, VirtualMachine.MEMORY_SIZE)
      disassembler = new MixDisassembler(processingModel)
    }
  }

  def switchOffMachine(): Unit = {
    lines = Vector.empty
    _machine = None
    _symbols = Vector.empty
    _addressSymbols = Vector.empty
  }

  def isActive: Boolean = _machine.isDefined

  def canMoveForward: Boolean = _machine.exists(_.canMoveForward)

  def runForward(): Future[Unit] = _machine match {
    case Some(m) =>
      m.runForward().map(stateChange.next)
    case None => throw new Error
  }

  def stepForward(): Future[Unit] = _machine match {
    case Some(m) =>
      m.stepForward().map(stateChange.next)
    case None => throw new Error
  }

  def canMoveBack: Boolean = _machine.exists({
    case m: TrackingVirtualMachine => m.canMoveBack
    case _ => false
  })

  def runBack(): Unit = _machine match {
    case Some(m: TrackingVirtualMachine) =>
      m.runBack()
      stateChange.next(Unit)
    case _ => throw new Error
  }

  def stepBack(): Unit = _machine match {
    case Some(m: TrackingVirtualMachine) =>
      m.stepBack()
      stateChange.next(Unit)
    case _ => throw new Error
  }

  def machineState: Option[VirtualMachineState] = _machine.map(_.currentState)

  def symbolsLength: Int = _symbols.length

  def symbolIndices: Range = _symbols.indices

  def programCounterIndex: Option[Int] = machineState.map(s => _addressSymbols(s.getProgramCounter))

  def toggleBreakpointAt(index: Int): Unit = _machine match {
    case Some(m) => addressAt(index).foreach(m.toggleBreakpoint)
    case None => throw new Error
  }

  def addressAt(index: Int): Option[Short] = _symbols(index)._1

  def breakpointAt(index: Int): Option[Boolean] = _machine.map(m => addressAt(index).exists(m.breakpointAt))

  def cellContent(index: Int): Option[IOWord] = {
    val address = addressAt(index)
    machineState.flatMap(s => address.map(s.get))
  }

  def lineNumberAt(index: Int): Option[Int] = _symbols(index)._2

  def lineAt(index: Int): Option[String] = _machine.flatMap(m => {
    val (address, lineNumber) = _symbols(index)
    lineNumber.map(ln =>
      if (address.exists(m.isModified)) {
        val cellContent = m.currentState.get(address.get)
        disassembler.disassembleLine(cellContent)
      } else lines(ln)
    )
  })

  def lineIsModifiedAt(index: Int): Option[Boolean] = _machine.map(m => {
    val address = addressAt(index)
    address.exists(m.isModified)
  })

  def deviceNumbers: Iterable[Int] = VirtualMachineService.deviceNames.keys

  def deviceName(deviceNum: Int): String = VirtualMachineService.deviceNames(deviceNum)

  def blockDeviceData(deviceNum: Int): Future[IndexedSeq[IOWord]] = device(deviceNum) match {
    case Some(d: BlockDevice) => d.data
    case _ => VirtualMachineService.getBlockDeviceData(deviceNum)
  }

  private def device(deviceNum: Int): Option[Device] =
    _machine.map(_.currentState.getDevice(deviceNum))

  def lineDeviceData(deviceNum: Int): Future[IndexedSeq[String]] =    device(deviceNum) match {
      case Some(d: LineDevice) => d.data
      case _ => VirtualMachineService.getLineDeviceData(deviceNum)
    }

}

object VirtualMachineService {
  private val goDevice = 16

  private val deviceNames: Map[Int, String] = SortedMap(
    0 -> "Tape Unit 0",
    1 -> "Tape Unit 1",
    2 -> "Tape Unit 2",
    3 -> "Tape Unit 3",
    4 -> "Tape Unit 4",
    5 -> "Tape Unit 5",
    6 -> "Tape Unit 6",
    7 -> "Tape Unit 7",
    8 -> "Disk Unit 0",
    9 -> "Disk Unit 1",
    10 -> "Disk Unit 2",
    11 -> "Disk Unit 3",
    12 -> "Disk Unit 4",
    13 -> "Disk Unit 5",
    14 -> "Disk Unit 6",
    15 -> "Disk Unit 7",
    16 -> "Card Reader",
    17 -> "Card Punch",
    18 -> "Line Printer",
    20 -> "Paper Tape"
  )

  private def createDevices(): Map[Int, Device] = Map(
    0 -> FileTapeUnit.create("device0"),
    1 -> FileTapeUnit.create("device1"),
    2 -> FileTapeUnit.create("device2"),
    3 -> FileTapeUnit.create("device3"),
    4 -> FileTapeUnit.create("device4"),
    5 -> FileTapeUnit.create("device5"),
    6 -> FileTapeUnit.create("device6"),
    7 -> FileTapeUnit.create("device7"),
    8 -> FileDiskUnit.create("device8"),
    9 -> FileDiskUnit.create("device9"),
    10 -> FileDiskUnit.create("device10"),
    11 -> FileDiskUnit.create("device11"),
    12 -> FileDiskUnit.create("device12"),
    13 -> FileDiskUnit.create("device13"),
    14 -> FileDiskUnit.create("device14"),
    15 -> FileDiskUnit.create("device15"),
    16 -> FileCardReader.create("device16"),
    17 -> FileCardPunch.create("device17"),
    18 -> FileLinePrinter.create("device18"),
    20 -> FilePaperTape.create("device20")
  )

  def getBlockDeviceData(deviceNum: Int): Future[IndexedSeq[IOWord]] = deviceNum match {
    case i if i >= 0 && i < 16 => FileBlockIODevice.getCurrentData(s"device$i")
    case _ => throw new Error
  }

  def saveBlockDevice(deviceNum: Int, data: Array[Byte]): Future[Unit] = {
    val device = deviceNum match {
      case i if i >= 0 && i < 8 => FileTapeUnit.create(s"device$i", data)
      case i if i >= 8 && i < 16 => FileDiskUnit.create(s"device$i", data)
      case _ => throw new Error
    }
    device.task.map(_ => ())
  }

  def getLineDeviceData(deviceNum: Int): Future[IndexedSeq[String]] = deviceNum match {
    case 16 => FileLineInputDevice.getCurrentData("device16")
    case 17 => FileLineOutputDevice.getCurrentData("device17")
    case 18 => FileLineOutputDevice.getCurrentData("device18")
    case 20 => FileLineInputDevice.getCurrentData("device20")
    case _ => throw new Error
  }

  def saveLineDevice(deviceNum: Int, data: String): Future[Unit] = {
    val device = deviceNum match {
      case 16 => FileCardReader.create("device16", data)
      case 20 => FilePaperTape.create("device20", data)
      case _ => throw new Error
    }
    device.task.map(_ => ())
  }
}
