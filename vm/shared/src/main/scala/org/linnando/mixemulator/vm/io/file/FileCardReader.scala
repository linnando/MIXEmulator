package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.CardReader

import scala.collection.immutable.Queue
import scala.concurrent.Future

case class FileCardReader(filename: String,
                          tasks: Future[Queue[IndexedSeq[IOWord]]] = Future.successful(Queue.empty),
                          isBusy: Boolean = false,
                          pos: Long = 0L)
  extends CardReader with FileLineInputDevice {

  override def blockSize: Int = CardReader.BLOCK_SIZE

  override def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileCardReader =
    copy(tasks = tasks, isBusy = true, pos = pos + 1L)

  override def withoutTasks: FileCardReader =
    copy(tasks = Future.successful(Queue.empty), isBusy = false)

}
