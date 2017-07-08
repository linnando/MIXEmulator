package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.TapeUnit
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.Future

case class FileTapeUnit(filename: String,
                        version: Int = 0,
                        tasks: Future[Queue[IndexedSeq[IOWord]]] = Future.successful(Queue.empty),
                        isBusy: Boolean = false,
                        pos: Long = 0L)
  extends TapeUnit with FileSequentialBlockIODevice {

  override def blockSize: Int = TapeUnit.BLOCK_SIZE

  override def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileTapeUnit =
    copy(tasks = tasks, isBusy = true, pos = pos + 1L)

  override def newVersion(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileTapeUnit =
    copy(version = version + 1, tasks = tasks, isBusy = true, pos = pos + 1L)

  override def withoutTasks: FileTapeUnit =
    copy(tasks = Future.successful(Queue.empty), isBusy = false)

  override def positioned(offset: Long): FileTapeUnit = copy(pos = pos + offset)

}
