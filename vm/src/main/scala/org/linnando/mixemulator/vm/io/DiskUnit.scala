package org.linnando.mixemulator.vm.io

trait DiskUnit extends RandomAccessIODevice {
  def positioned(pos: Long): DiskUnit
}

object DiskUnit {
  val BLOCK_SIZE = 100
}
