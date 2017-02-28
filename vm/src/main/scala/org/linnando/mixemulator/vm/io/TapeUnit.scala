package org.linnando.mixemulator.vm.io

trait TapeUnit extends SequentialIODevice {
  def positioned(pos: Long): TapeUnit
}

object TapeUnit {
  val BLOCK_SIZE = 100
}
