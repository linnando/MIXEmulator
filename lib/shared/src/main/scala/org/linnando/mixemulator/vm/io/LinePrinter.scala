package org.linnando.mixemulator.vm.io

trait LinePrinter extends LineDevice with PositionalOutputDevice {
  def newPage(): LinePrinter
}

object LinePrinter {
  val BLOCK_SIZE = 24
}
