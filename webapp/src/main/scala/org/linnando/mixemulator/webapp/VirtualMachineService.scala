package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.asm.{MixAssembler, MixDisassembler}
import org.linnando.mixemulator.vm._
import org.linnando.mixemulator.vm.io.{BlockDevice, Device, LineDevice}
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.file._
import rxjs.Subject

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Injectable()
class VirtualMachineService {
  var text: String =
    """START      OUT  HELLO(18)
           JBUS *
           HLT
HELLO      ALF  HELLO
           ALF  , WOR
           ALF  LD
           END START"""

  val stateChange: Subject[Unit] = new Subject()

  private var lines: Vector[String] = Vector.empty

  private var _machine: Option[VirtualMachine] = None

  private var _symbols: Vector[(Option[Short], Option[Int])] = Vector.empty

  private var _addressSymbols: Vector[Int] = Vector.empty

  private var disassembler: MixDisassembler = _

  def assembleBinaryNonTracking(): Future[Unit] = Future {
    lines = text.split("\n").toVector
    val builder = binary.createVirtualMachineBuilder()
      .withDevices(VirtualMachineService.devices.mapValues(_._2()))
    saveAssembly(MixAssembler.translateNonTracking(builder, lines))
    disassembler = new MixDisassembler(binary)
  }

  private def saveAssembly(assembly: (VirtualMachine, List[(Option[Short], Option[Int])])): Unit = {
    _machine = Some(assembly._1)
    _symbols = assembly._2.toVector
    _addressSymbols = _symbols.indices.foldLeft(Vector.fill(VirtualMachine.MEMORY_SIZE)(0)) { (s, index) =>
      _symbols(index) match {
        case (Some(address), _) => s.updated(address, index)
        case (None, _) => s
      }
    }
  }

  def assembleBinaryTracking(): Future[Unit] = Future {
    lines = text.split("\n").toVector
    val builder = binary.createVirtualMachineBuilder()
      .withDevices(VirtualMachineService.devices.mapValues(_._2()))
    saveAssembly(MixAssembler.translateTracking(builder, lines))
    disassembler = new MixDisassembler(binary)
  }

  def assembleDecimalNonTracking(): Future[Unit] = Future {
    lines = text.split("\n").toVector
    val builder = decimal.createVirtualMachineBuilder()
      .withDevices(VirtualMachineService.devices.mapValues(_._2()))
    saveAssembly(MixAssembler.translateNonTracking(builder, lines))
    disassembler = new MixDisassembler(binary)
  }

  def assembleDecimalTracking(): Future[Unit] = Future {
    lines = text.split("\n").toVector
    val builder = decimal.createVirtualMachineBuilder()
      .withDevices(VirtualMachineService.devices.mapValues(_._2()))
    saveAssembly(MixAssembler.translateTracking(builder, lines))
    disassembler = new MixDisassembler(binary)
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

  def breakpointAt(index: Int): Option[Boolean] = _machine.map(m => addressAt(index).exists(m.breakpoints))

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

  def deviceNumbers: Iterable[Int] = VirtualMachineService.devices.keys

  def deviceName(deviceNum: Int): String = VirtualMachineService.devices(deviceNum)._1

  def lineDeviceData(deviceNum: Int): Future[IndexedSeq[String]] = device(deviceNum) match {
    case d: LineDevice => d.data
    case _ => throw new Error
  }

  private def device(deviceNum: Int) = _machine match {
    case Some(m) => m.currentState.getDevice(deviceNum)
    case None => VirtualMachineService.devices(deviceNum)._2()
  }

  def saveLineDevice(deviceNum: Int, data: String): Future[Unit] = {
    val device = deviceNum match {
      case 16 => FileCardReader.create("device16", data)
      case 20 => FilePaperTape.create("device20", data)
      case _ => throw new Error
    }
    device.tasks.map(_ => ())
  }

  def blockDeviceData(deviceNum: Int): Future[IndexedSeq[IOWord]] = device(deviceNum) match {
    case d: BlockDevice => d.data
    case _ => throw new Error
  }

  def saveBlockDevice(deviceNum: Int, data: Array[Byte]): Future[Unit] = {
    val device = deviceNum match {
      case i if i >= 0 && i < 8 => FileTapeUnit.create(s"device$i", data)
      case i if i >= 8 && i < 16 => FileDiskUnit.create(s"device$i", data)
      case _ => throw new Error
    }
    device.tasks.map(_ => ())
  }
}

object VirtualMachineService {
  private val devices: Map[Int, (String, () => Device)] = SortedMap(
    0 -> ("Tape Unit 0", () => FileTapeUnit.create("device0")),
    1 -> ("Tape Unit 1", () => FileTapeUnit.create("device1")),
    2 -> ("Tape Unit 2", () => FileTapeUnit.create("device2")),
    3 -> ("Tape Unit 3", () => FileTapeUnit.create("device3")),
    4 -> ("Tape Unit 4", () => FileTapeUnit.create("device4")),
    5 -> ("Tape Unit 5", () => FileTapeUnit.create("device5")),
    6 -> ("Tape Unit 6", () => FileTapeUnit.create("device6")),
    7 -> ("Tape Unit 7", () => FileTapeUnit.create("device7")),
    8 -> ("Disk Unit 0", () => FileDiskUnit.create("device8")),
    9 -> ("Disk Unit 1", () => FileDiskUnit.create("device9")),
    10 -> ("Disk Unit 2", () => FileDiskUnit.create("device10")),
    11 -> ("Disk Unit 3", () => FileDiskUnit.create("device11")),
    12 -> ("Disk Unit 4", () => FileDiskUnit.create("device12")),
    13 -> ("Disk Unit 5", () => FileDiskUnit.create("device13")),
    14 -> ("Disk Unit 6", () => FileDiskUnit.create("device14")),
    15 -> ("Disk Unit 7", () => FileDiskUnit.create("device15")),
    16 -> ("Card Reader", () => FileCardReader.create("device16")),
    17 -> ("Card Punch", () => FileCardPunch.create("device17")),
    18 -> ("Line Printer", () => FileLinePrinter.create("device18")),
    20 -> ("Paper Tape", () => FilePaperTape.create("device20"))
  )
}
