package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, PositionalOutputDevice}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class MockPositionalOutputDevice(counter: Int = 0,
                                      block: IndexedSeq[IOWord] = IndexedSeq.empty,
                                      busy: Boolean = false) extends PositionalOutputDevice {
  override def write(words: IndexedSeq[IOWord]): PositionalOutputDevice =
    copy(counter = counter + 1, block = words, busy = true)

  override def blockSize: Int = MockPositionalOutputDevice.blockSize

  override def isBusy: Boolean = busy

  override def flush(): Future[(Device, Option[IndexedSeq[IOWord]])] = Future {
    (copy(busy = false), None)
  }
}

object MockPositionalOutputDevice {
  val blockSize: Int = 100
}