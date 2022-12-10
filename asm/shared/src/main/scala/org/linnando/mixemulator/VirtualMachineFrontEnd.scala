package org.linnando.mixemulator

import org.linnando.mixemulator.asm.{MixAssembler, MixDisassembler}
import org.linnando.mixemulator.vm.io.{BlockDevice, Device, LineDevice}
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.file.{FileBlockIODevice, FileCardPunch, FileCardReader, FileDiskUnit, FileLineInputDevice, FileLineOutputDevice, FileLinePrinter, FilePaperTape, FileTapeUnit}
import org.linnando.mixemulator.vm.{TrackingVirtualMachine, VirtualMachine, VirtualMachineState, VirtualMachineStateJs, binary, decimal}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("VirtualMachineFrontEnd")
class VirtualMachineFrontEnd(machine: VirtualMachine,
                             lines: IndexedSeq[String],
                             symbols: IndexedSeq[(Option[Short], Option[Int])],
                             disassembler: MixDisassembler) {
  private val addressSymbols: Vector[Int] =
    symbols.indices.foldLeft(Vector.fill(VirtualMachine.MEMORY_SIZE)(0)) { (s, index) =>
      val address = symbols(index)._1
      if (address.isEmpty) s
      else s.updated(address.get, index)
    }

  def currentState: VirtualMachineState = machine.currentState

  @JSExport("currentState")
  def currentStateJs: VirtualMachineStateJs = new VirtualMachineStateJs(currentState)

  @JSExport
  def canMoveForward: Boolean = machine.canMoveForward

  def stepForward(): Future[Unit] = machine.stepForward()

  @JSExport("stepForward")
  def stepForwardJs(): js.Promise[Unit] = stepForward().toJSPromise

  def runForward(): Future[Unit] = machine.runForward()

  @JSExport("runForward")
  def runForwardJs(): js.Promise[Unit] = runForward().toJSPromise

  @JSExport
  def canMoveBack: Boolean = machine match {
    case m: TrackingVirtualMachine => m.canMoveBack
    case _ => false
  }

  @JSExport
  def stepBack(): Unit = machine match {
    case m: TrackingVirtualMachine => m.stepBack()
    case _ => throw new Error
  }

  @JSExport
  def runBack(): Unit = machine match {
    case m: TrackingVirtualMachine => m.runBack()
    case _ => throw new Error
  }

  @JSExport
  def symbolsLength: Int = symbols.length

  def symbolIndices: Range = symbols.indices

  @JSExport("symbolIndices")
  def symbolIndicesJs: js.Array[Int] = symbolIndices.toJSArray

  @JSExport
  def programCounterIndex: Int = addressSymbols(currentState.getProgramCounter)

  @JSExport
  def toggleBreakpointAt(index: Int): Unit = addressAt(index).foreach(machine.toggleBreakpoint)

  def addressAt(index: Int): Option[Short] = symbols(index)._1

  @JSExport("addressAt")
  def addressAtJs(index: Int): js.UndefOr[Short] = addressAt(index).orUndefined

  @JSExport
  def breakpointAt(index: Int): Boolean = addressAt(index).exists(machine.breakpointAt)

  def cellContent(index: Int): Option[IOWord] = addressAt(index).map(currentState.get)

  @JSExport("cellContent")
  def cellContentJs(index: Int): js.UndefOr[IOWord] = cellContent(index).orUndefined

  def lineNumberAt(index: Int): Option[Int] = symbols(index)._2

  @JSExport("lineNumberAt")
  def lineNumberAtJs(index: Int): js.UndefOr[Int] = lineNumberAt(index).orUndefined

  def lineAt(index: Int): Option[String] = {
    val (address, lineNumber) = symbols(index)
    lineNumber.map(ln =>
      if (address.exists(machine.isModified)) {
        val cellContent = machine.currentState.get(address.get)
        disassembler.disassembleLine(cellContent)
      } else lines(ln)
    )
  }

  @JSExport("lineAt")
  def lineAtJs(index: Int): js.UndefOr[String] = lineAt(index).orUndefined

  @JSExport
  def lineIsModifiedAt(index: Int): Boolean = addressAt(index).exists(machine.isModified)

  def blockDeviceData(deviceNum: Int): Future[IndexedSeq[IOWord]] = device(deviceNum) match {
    case d: BlockDevice => d.data
    case _ => throw new Error
  }

  @JSExport("blockDeviceData")
  def blockDeviceDataJs(deviceNum: Int): js.Promise[js.Array[IOWord]] =
    blockDeviceData(deviceNum).map(_.toJSArray).toJSPromise

  private def device(deviceNum: Int): Device = currentState.getDevice(deviceNum)

  def lineDeviceData(deviceNum: Int): Future[IndexedSeq[String]] = device(deviceNum) match {
    case d: LineDevice => d.data
    case _ => throw new Error
  }

  @JSExport("lineDeviceData")
  def lineDeviceDataJs(deviceNum: Int): js.Promise[js.Array[String]] =
    lineDeviceData(deviceNum).map(_.toJSArray).toJSPromise
}

@JSExportTopLevel("VirtualMachineFrontEnd$")
object VirtualMachineFrontEnd {
  def createBinaryTracking(lines: Seq[String]): VirtualMachineFrontEnd = {
    val builder = binary.createVirtualMachineBuilder()
      .withDevices(createDevices())
    val assembly = MixAssembler.translateTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(binary))
  }

  @JSExport("createBinaryTracking")
  def createBinaryTrackingJs(lines: js.Array[String]): VirtualMachineFrontEnd = createBinaryTracking(lines.toSeq)

  def createBinaryNonTracking(lines: Seq[String]): VirtualMachineFrontEnd = {
    val builder = binary.createVirtualMachineBuilder()
      .withDevices(createDevices())
    val assembly = MixAssembler.translateNonTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(binary))
  }

  @JSExport("createBinaryNonTracking")
  def createBinaryNonTrackingJs(lines: js.Array[String]): VirtualMachineFrontEnd =
    createBinaryNonTracking(lines.toSeq)

  def createDecimalTracking(lines: Seq[String]): VirtualMachineFrontEnd = {
    val builder = decimal.createVirtualMachineBuilder()
      .withDevices(createDevices())
    val assembly = MixAssembler.translateTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(decimal))
  }

  @JSExport("createDecimalTracking")
  def createDecimalTrackingJs(lines: js.Array[String]): VirtualMachineFrontEnd = createDecimalTracking(lines.toSeq)

  def createDecimalNonTracking(lines: Seq[String]): VirtualMachineFrontEnd = {
    val builder = decimal.createVirtualMachineBuilder()
      .withDevices(createDevices())
    val assembly = MixAssembler.translateNonTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(decimal))
  }

  @JSExport("createDecimalNonTracking")
  def createDecimalNonTrackingJs(lines: js.Array[String]): VirtualMachineFrontEnd =
    createDecimalNonTracking(lines.toSeq)

  def goBinaryTracking(): Future[VirtualMachineFrontEnd] = {
    val devices = createDevices()
    binary.goTracking(devices, goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(binary)
      )
    }
  }

  @JSExport("goBinaryTracking")
  def goBinaryTrackingJs(): js.Promise[VirtualMachineFrontEnd] = goBinaryTracking().toJSPromise

  def goBinaryNonTracking(): Future[VirtualMachineFrontEnd] = {
    val devices = createDevices()
    binary.goNonTracking(devices, goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(binary)
      )
    }
  }

  @JSExport("goBinaryNonTracking")
  def goBinaryNonTrackingJs(): js.Promise[VirtualMachineFrontEnd] = goBinaryNonTracking().toJSPromise

  def goDecimalTracking(): Future[VirtualMachineFrontEnd] = {
    val devices = createDevices()
    decimal.goTracking(devices, goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(decimal)
      )
    }
  }

  @JSExport("goDecimalTracking")
  def goDecimalTrackingJs(): js.Promise[VirtualMachineFrontEnd] = goDecimalTracking().toJSPromise

  def goDecimalNonTracking(): Future[VirtualMachineFrontEnd] = {
    val devices = createDevices()
    decimal.goNonTracking(devices, goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(decimal)
      )
    }
  }

  @JSExport("goDecimalNonTracking")
  def goDecimalNonTrackingJs(): js.Promise[VirtualMachineFrontEnd] = goDecimalNonTracking().toJSPromise

  private val goDevice = 16

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

  @JSExport("getBlockDeviceData")
  def getBlockDeviceDataJs(deviceNum: Int): js.Promise[js.Array[IOWord]] =
    getBlockDeviceData(deviceNum).map(_.toJSArray).toJSPromise

  def saveBlockDevice(deviceNum: Int, data: Array[Byte]): Future[Unit] = {
    val device = deviceNum match {
      case i if i >= 0 && i < 8 => FileTapeUnit.create(s"device$i", data)
      case i if i >= 8 && i < 16 => FileDiskUnit.create(s"device$i", data)
      case _ => throw new Error
    }
    device.task.map(_ => ())
  }

  @JSExport("saveBlockDevice")
  def saveBlockDeviceJs(deviceNum: Int, data: js.Array[Byte]): js.Promise[Unit] =
    saveBlockDevice(deviceNum, data.toArray).toJSPromise

  def getLineDeviceData(deviceNum: Int): Future[IndexedSeq[String]] = deviceNum match {
    case 16 => FileLineInputDevice.getCurrentData("device16")
    case 17 => FileLineOutputDevice.getCurrentData("device17")
    case 18 => FileLineOutputDevice.getCurrentData("device18")
    case 20 => FileLineInputDevice.getCurrentData("device20")
    case _ => throw new Error
  }

  @JSExport("getLineDeviceData")
  def getLineDeviceDataJs(deviceNum: Int): js.Promise[js.Array[String]] =
    getLineDeviceData(deviceNum).map(_.toJSArray).toJSPromise

  def saveLineDevice(deviceNum: Int, data: String): Future[Unit] = {
    val device = deviceNum match {
      case 16 => FileCardReader.create("device16", data)
      case 20 => FilePaperTape.create("device20", data)
      case _ => throw new Error
    }
    device.task.map(_ => ())
  }

  @JSExport("saveLineDevice")
  def saveLineDeviceJs(deviceNum: Int, data: String): js.Promise[Unit] =
    saveLineDevice(deviceNum, data).toJSPromise
}
