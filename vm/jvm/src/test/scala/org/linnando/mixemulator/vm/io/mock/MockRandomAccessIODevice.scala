package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, RandomAccessIODevice}

case class MockRandomAccessIODevice(position: Long = 0L,
                                    block: IndexedSeq[IOWord] = IndexedSeq.empty,
                                    busy: Boolean = false) extends RandomAccessIODevice {
  override def read(pos: Long): RandomAccessIODevice = copy(position = pos, busy = true)

  override def write(pos: Long, words: IndexedSeq[IOWord]): RandomAccessIODevice =
    copy(position = pos, block = words, busy = true)

  override def blockSize: Int = 100

  override def isBusy: Boolean = busy

  override def flush(): (Device, Seq[IndexedSeq[IOWord]]) = (
    copy(busy = false),
    Seq(IndexedSeq.fill(blockSize)(IOWord(negative = false, Seq(1, 1, 1, 1, 1))))
  )
}