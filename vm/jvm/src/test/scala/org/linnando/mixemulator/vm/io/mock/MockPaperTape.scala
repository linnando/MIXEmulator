package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, PaperTape, PositionalInputDevice}

import scala.concurrent.Future

case class MockPaperTape(counter: Int = 0) extends PaperTape {
  override def reset(): PaperTape = copy(counter = counter + 1)

  override def read(): PositionalInputDevice = ???

  override def blockSize: Int = ???

  override def isBusy: Boolean = ???

  override def flush(): Future[(Device, Seq[IndexedSeq[IOWord]])] = ???
}
