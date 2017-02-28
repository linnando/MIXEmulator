package org.linnando.mixemulator.vm.io

trait LinePrinter extends PositionalOutputDevice {
  def newPage(): LinePrinter
}

object LinePrinter {
  val BLOCK_SIZE = 24
}
