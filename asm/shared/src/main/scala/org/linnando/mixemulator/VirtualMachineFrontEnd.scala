package org.linnando.mixemulator

import org.linnando.mixemulator.asm.{MixAssembler, MixDisassembler}
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{BlockDevice, Device, LineDevice}
import org.linnando.mixemulator.vm._

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
  private val goDevice = 16

  def createBinaryTracking(lines: Seq[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd = {
    val builder = binary.createVirtualMachineBuilder().withDevices(devicesFrontEnd.createDevices())
    val assembly = MixAssembler.translateTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(binary))
  }

  @JSExport("createBinaryTracking")
  def createBinaryTrackingJs(lines: js.Array[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd =
    createBinaryTracking(lines.toSeq, devicesFrontEnd)

  def createBinaryNonTracking(lines: Seq[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd = {
    val builder = binary.createVirtualMachineBuilder().withDevices(devicesFrontEnd.createDevices())
    val assembly = MixAssembler.translateNonTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(binary))
  }

  @JSExport("createBinaryNonTracking")
  def createBinaryNonTrackingJs(lines: js.Array[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd =
    createBinaryNonTracking(lines.toSeq, devicesFrontEnd)

  def createDecimalTracking(lines: Seq[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd = {
    val builder = decimal.createVirtualMachineBuilder().withDevices(devicesFrontEnd.createDevices())
    val assembly = MixAssembler.translateTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(decimal))
  }

  @JSExport("createDecimalTracking")
  def createDecimalTrackingJs(lines: js.Array[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd =
    createDecimalTracking(lines.toSeq, devicesFrontEnd)

  def createDecimalNonTracking(lines: Seq[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd = {
    val builder = decimal.createVirtualMachineBuilder().withDevices(devicesFrontEnd.createDevices())
    val assembly = MixAssembler.translateNonTracking(builder, lines)
    new VirtualMachineFrontEnd(assembly._1, lines.toVector, assembly._2.toVector, new MixDisassembler(decimal))
  }

  @JSExport("createDecimalNonTracking")
  def createDecimalNonTrackingJs(lines: js.Array[String], devicesFrontEnd: DevicesFrontEnd): VirtualMachineFrontEnd =
    createDecimalNonTracking(lines.toSeq, devicesFrontEnd)

  def goBinaryTracking(devicesFrontEnd: DevicesFrontEnd): Future[VirtualMachineFrontEnd] =
    binary.goTracking(devicesFrontEnd.createDevices(), goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(binary)
      )
    }

  @JSExport("goBinaryTracking")
  def goBinaryTrackingJs(devicesFrontEnd: DevicesFrontEnd): js.Promise[VirtualMachineFrontEnd] =
    goBinaryTracking(devicesFrontEnd).toJSPromise

  def goBinaryNonTracking(devicesFrontEnd: DevicesFrontEnd): Future[VirtualMachineFrontEnd] =
    binary.goNonTracking(devicesFrontEnd.createDevices(), goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(binary)
      )
    }

  @JSExport("goBinaryNonTracking")
  def goBinaryNonTrackingJs(devicesFrontEnd: DevicesFrontEnd): js.Promise[VirtualMachineFrontEnd] =
    goBinaryNonTracking(devicesFrontEnd).toJSPromise

  def goDecimalTracking(devicesFrontEnd: DevicesFrontEnd): Future[VirtualMachineFrontEnd] =
    decimal.goTracking(devicesFrontEnd.createDevices(), goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(decimal)
      )
    }

  @JSExport("goDecimalTracking")
  def goDecimalTrackingJs(devicesFrontEnd: DevicesFrontEnd): js.Promise[VirtualMachineFrontEnd] =
    goDecimalTracking(devicesFrontEnd).toJSPromise

  def goDecimalNonTracking(devicesFrontEnd: DevicesFrontEnd): Future[VirtualMachineFrontEnd] =
    decimal.goNonTracking(devicesFrontEnd.createDevices(), goDevice) map { machine =>
      new VirtualMachineFrontEnd(
        machine,
        Vector.empty,
        Vector.tabulate(VirtualMachine.MEMORY_SIZE)(i => (Some(i.toShort), None)),
        new MixDisassembler(decimal)
      )
    }

  @JSExport("goDecimalNonTracking")
  def goDecimalNonTrackingJs(devicesFrontEnd: DevicesFrontEnd): js.Promise[VirtualMachineFrontEnd] =
    goDecimalNonTracking(devicesFrontEnd).toJSPromise
}
