package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.TapeUnit
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.Future

case class FileTapeUnit(filename: String, version: Int, tasks: Future[Queue[IndexedSeq[IOWord]]], isBusy: Boolean, pos: Long)
  extends TapeUnit with FileSequentialBlockIODevice {

  override def blockSize: Int = TapeUnit.BLOCK_SIZE

  override def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileTapeUnit =
    copy(tasks = tasks, isBusy = true, pos = pos + blockSize)

  override def newVersion(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileTapeUnit =
    copy(version = version + 1, tasks = tasks, isBusy = true, pos = pos + blockSize)

  override def withoutTasks: FileTapeUnit =
    copy(tasks = Future.successful(Queue.empty), isBusy = false)

  override def positioned(offset: Long): FileTapeUnit = copy(pos = pos + offset)

}
