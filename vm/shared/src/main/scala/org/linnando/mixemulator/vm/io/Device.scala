package org.linnando.mixemulator.vm.io

import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.Future

trait Device {
  def blockSize: Int
  def isBusy: Boolean
  def flush(): Future[(Device, Seq[IndexedSeq[IOWord]])]
}

trait LineDevice extends Device {
  def data: Future[IndexedSeq[String]]
}

trait BlockDevice extends Device {
  def data: Future[IndexedSeq[IOWord]]
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
