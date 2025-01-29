package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, PositionalInputDevice}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class MockPositionalInputDevice(counter: Int = 0, busy: Boolean = false) extends PositionalInputDevice {
  override def read(): PositionalInputDevice = copy(counter = counter + 1, busy = true)

  override def blockSize: Int = MockPositionalInputDevice.blockSize

  override def isBusy: Boolean = busy

  override def flush(): Future[(Device, Option[IndexedSeq[IOWord]])] = Future {
    if (isBusy) {
      val ioWords = IndexedSeq.fill(blockSize)(IOWord(negative = false, Seq(1, 1, 1, 1, 1)))
      (copy(busy = false), Some(ioWords))
    } else (this, None)
  }
}

object MockPositionalInputDevice {
  val blockSize: Int = 100
}
