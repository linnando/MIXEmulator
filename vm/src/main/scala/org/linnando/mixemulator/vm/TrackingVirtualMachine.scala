package org.linnando.mixemulator.vm

trait TrackingVirtualMachine extends VirtualMachine {
  def canMoveBack: Boolean

  def stepBack(): Unit

  def runBack(): Unit
}
