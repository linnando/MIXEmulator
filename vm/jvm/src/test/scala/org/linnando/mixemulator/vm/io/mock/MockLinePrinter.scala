package org.linnando.mixemulator.vm.io.mock

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.{Device, LinePrinter, PositionalOutputDevice}

import scala.concurrent.Future

case class MockLinePrinter(page: Int = 0) extends LinePrinter {
  override def newPage(): LinePrinter = copy(page = page + 1)

  override def write(words: IndexedSeq[IOWord]): PositionalOutputDevice = ???

  override def blockSize: Int = ???

  override def isBusy: Boolean = ???

  override def flush(): Future[(Device, Seq[IndexedSeq[IOWord]])] = ???
}
