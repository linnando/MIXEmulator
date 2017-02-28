package org.linnando.mixemulator.vm.io

trait PaperTape extends PositionalInputDevice {
  def reset(): PaperTape
}

object PaperTape {
  val BLOCK_SIZE = 14
}
