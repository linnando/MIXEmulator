package org.linnando.mixemulator.vm

import org.linnando.mixemulator.vm.io.Device
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

trait VirtualMachineState {
  def get(address: Short): IOWord

  def getA: IOWord

  def getX: IOWord

  def getI1: IOWord

  def getI2: IOWord

  def getI3: IOWord

  def getI4: IOWord

  def getI5: IOWord

  def getI6: IOWord

  def getJ: IOWord

  def getOV: Boolean

  def getCMP: Comparison.Value

  def getProgramCounter: Short

  def getTimeCounter: Int

  def getDevice(deviceNum: Int): Device

  def isHalted: Boolean
}

@JSExportTopLevel("VirtualMachineState")
class VirtualMachineStateJs(state: VirtualMachineState) {
  @JSExport
  def get(address: Short): IOWord = state.get(address)

  @JSExport
  def getA: IOWord = state.getA

  @JSExport
  def getX: IOWord = state.getX

  @JSExport
  def getI1: IOWord = state.getI1

  @JSExport
  def getI2: IOWord = state.getI2

  @JSExport
  def getI3: IOWord = state.getI3

  @JSExport
  def getI4: IOWord = state.getI4

  @JSExport
  def getI5: IOWord = state.getI5

  @JSExport
  def getI6: IOWord = state.getI6

  @JSExport
  def getJ: IOWord = state.getJ

  @JSExport
  def getOV: Boolean = state.getOV

  @JSExport
  def getCMP: Comparison.Value = state.getCMP

  @JSExport
  def getTimeCounter: Int = state.getTimeCounter

  @JSExport
  def isHalted: Boolean = state.isHalted
}
