package org.linnando.mixemulator.vm.io
import org.linnando.mixemulator.vm.io.data.IOWord

case class MockPaperTape(counter: Int = 0) extends PaperTape {
  override def reset(): PaperTape = copy(counter = counter + 1)

  override def read(): PositionalInputDevice = ???

  override def blockSize: Int = ???

  override def isBusy: Boolean = ???

  override def flush(): (Device, Seq[IndexedSeq[IOWord]]) = ???
}
