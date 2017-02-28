package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.DiskUnit
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.Future

case class FileDiskUnit(filename: String, version: Int, tasks: Future[Queue[IndexedSeq[IOWord]]], isBusy: Boolean)
  extends DiskUnit with FileRandomAccessBlockIODevice {

  override def blockSize = DiskUnit.BLOCK_SIZE

  override def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileDiskUnit =
    copy(tasks = tasks, isBusy = true)

  override def newVersion(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileDiskUnit =
    copy(version = version + 1, tasks = tasks, isBusy = true)

  override def withoutTasks: FileDiskUnit =
    copy(tasks = Future.successful(Queue.empty), isBusy = false)

  override def positioned(pos: Long): FileDiskUnit = this

}
