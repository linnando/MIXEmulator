package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, RandomAccessIODevice}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class MockRandomAccessIODevice(position: Long = 0L,
                                    block: IndexedSeq[IOWord] = IndexedSeq.empty,
                                    busy: Boolean = false,
                                    reading: Boolean = false) extends RandomAccessIODevice {
  override def read(pos: Long): RandomAccessIODevice = copy(position = pos, busy = true, reading = true)

  override def write(pos: Long, words: IndexedSeq[IOWord]): RandomAccessIODevice =
    copy(position = pos, block = words, busy = true)

  override def blockSize: Int = MockRandomAccessIODevice.blockSize

  override def isBusy: Boolean = busy

  override def flush(): Future[(Device, Option[IndexedSeq[IOWord]])] = Future {
    if (reading) {
      val ioWords = IndexedSeq.fill(blockSize)(IOWord(negative = false, Seq(1, 1, 1, 1, 1)))
      (copy(busy = false, reading = false), Some(ioWords))
    } else (copy(busy = false), None)
  }
}

object MockRandomAccessIODevice {
  val blockSize: Int = 100
}