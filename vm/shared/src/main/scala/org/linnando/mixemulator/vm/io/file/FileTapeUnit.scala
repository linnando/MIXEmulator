package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.TapeUnit
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FileTapeUnit(filename: String,
                        version: Int,
                        task: Future[Option[IndexedSeq[IOWord]]],
                        isBusy: Boolean,
                        pos: Long)
  extends TapeUnit with FileSequentialBlockIODevice {

  override def blockSize: Int = TapeUnit.BLOCK_SIZE

  override def withTasks(tasks: Future[Option[IndexedSeq[IOWord]]]): FileTapeUnit =
    copy(task = tasks, isBusy = true, pos = pos + 1L)

  override def newVersion(tasks: Future[Option[IndexedSeq[IOWord]]]): FileTapeUnit =
    copy(version = version + 1, task = tasks, isBusy = true, pos = pos + 1L)

  override def withoutTasks: FileTapeUnit =
    copy(task = Future.successful(None), isBusy = false)

  override def positioned(offset: Long): FileTapeUnit =
    copy(pos = if (offset == 0L) 0L else pos + offset)

}

object FileTapeUnit {
  def create(filename: String): FileTapeUnit =
    FileTapeUnit(filename, 0, FileBlockIODevice.initialise(filename).map(_ => None), isBusy = false, 0L)

  def create(filename: String, data: Array[Byte]): FileTapeUnit =
    FileTapeUnit(filename, 0, FileBlockIODevice.save(filename, data).map(_ => None), isBusy = false, 0L)
}
