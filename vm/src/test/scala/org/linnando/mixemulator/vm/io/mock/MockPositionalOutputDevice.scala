package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, PositionalOutputDevice}

case class MockPositionalOutputDevice(counter: Int = 0,
                                      block: IndexedSeq[IOWord] = IndexedSeq.empty,
                                      busy: Boolean = false) extends PositionalOutputDevice {
  override def write(words: IndexedSeq[IOWord]): PositionalOutputDevice =
    copy(counter = counter + 1, block = words, busy = true)

  override def blockSize: Int = 100

  override def isBusy: Boolean = busy

  override def flush(): (Device, Seq[IndexedSeq[IOWord]]) = (
    copy(busy = false),
    Seq.empty
  )
}
