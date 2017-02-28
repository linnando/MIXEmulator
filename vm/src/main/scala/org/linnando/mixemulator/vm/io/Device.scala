package org.linnando.mixemulator.vm.io

import org.linnando.mixemulator.vm.io.data.IOWord

trait Device {
  def blockSize: Int
  def isBusy: Boolean
  def flush(): (Device, Seq[IndexedSeq[IOWord]])
}

trait PositionalInputDevice extends Device {
  def read(): PositionalInputDevice
}

trait PositionalOutputDevice extends Device {
  def write(words: IndexedSeq[IOWord]): PositionalOutputDevice
}

trait SequentialIODevice extends PositionalInputDevice with PositionalOutputDevice {
}

trait RandomAccessIODevice extends Device {
  def read(pos: Long): RandomAccessIODevice
  def write(pos: Long, words: IndexedSeq[IOWord]): RandomAccessIODevice
}
