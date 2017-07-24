package org.linnando.mixemulator.vm

import org.linnando.mixemulator.vm.Comparison.Comparison
import org.linnando.mixemulator.vm.io.data.IOWord

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

  def getCMP: Comparison

  def getProgramCounter: Short

  def isHalted: Boolean
}
