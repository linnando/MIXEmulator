package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, PositionalInputDevice, PositionalOutputDevice, TapeUnit}

case class MockTapeUnit(position: Long = 0) extends TapeUnit {
  override def positioned(pos: Long): TapeUnit = copy(position = if (pos == 0) pos else position + pos)

  override def read(): PositionalInputDevice = ???

  override def write(words: IndexedSeq[IOWord]): PositionalOutputDevice = ???

  override def blockSize: Int = ???

  override def isBusy: Boolean = ???

  override def flush(): (Device, Seq[IndexedSeq[IOWord]]) = ???
}
