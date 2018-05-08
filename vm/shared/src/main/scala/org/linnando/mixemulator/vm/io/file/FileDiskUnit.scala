package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.DiskUnit
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FileDiskUnit(filename: String,
                        version: Int,
                        task: Future[Option[IndexedSeq[IOWord]]],
                        isBusy: Boolean)
  extends DiskUnit with FileRandomAccessBlockIODevice {

  override def blockSize: Int = DiskUnit.BLOCK_SIZE

  override def withTask(task: Future[Option[IndexedSeq[IOWord]]]): FileDiskUnit =
    copy(task = task, isBusy = true)

  override def newVersion(task: Future[Option[IndexedSeq[IOWord]]]): FileDiskUnit =
    copy(version = version + 1, task = task, isBusy = true)

  override def withoutTask: FileDiskUnit =
    copy(task = Future.successful(None), isBusy = false)

  override def positioned(pos: Long): FileDiskUnit = this
}

object FileDiskUnit {
  def create(filename: String): FileDiskUnit =
    FileDiskUnit(filename, 0, FileBlockIODevice.initialise(filename).map(_ => None), isBusy = false)

  def create(filename: String, data: Array[Byte]): FileDiskUnit =
    FileDiskUnit(filename, 0, FileBlockIODevice.save(filename, data).map(_ => None), isBusy = false)
}
