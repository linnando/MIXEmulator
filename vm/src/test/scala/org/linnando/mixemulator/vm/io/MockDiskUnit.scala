package org.linnando.mixemulator.vm.io
import org.linnando.mixemulator.vm.io.data.IOWord

case class MockDiskUnit(position: Long = 0) extends DiskUnit {
  override def positioned(pos: Long): DiskUnit = copy(position = pos)

  override def read(pos: Long): RandomAccessIODevice = ???

  override def write(pos: Long, words: IndexedSeq[IOWord]): RandomAccessIODevice = ???

  override def blockSize: Int = ???

  override def isBusy: Boolean = ???

  override def flush(): (Device, Seq[IndexedSeq[IOWord]]) = ???
}
