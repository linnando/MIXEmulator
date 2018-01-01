package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.PaperTape
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FilePaperTape(filename: String,
                         tasks: Future[Queue[IndexedSeq[IOWord]]],
                         isBusy: Boolean,
                         pos: Long)
  extends PaperTape with FileLineInputDevice {

  override def blockSize: Int = PaperTape.BLOCK_SIZE

  override def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FilePaperTape =
    copy(tasks = tasks, isBusy = true, pos = pos + 1L)

  override def withoutTasks: FilePaperTape =
    copy(tasks = Future.successful(Queue.empty), isBusy = false)

  override def reset(): FilePaperTape = copy(pos = 0L)
}

object FilePaperTape {
  def create(filename: String): FilePaperTape =
    FilePaperTape(filename, FileLineInputDevice.initialise(filename).map(_ => Queue.empty), isBusy = false, 0L)

  def create(filename: String, data: String): FilePaperTape =
    FilePaperTape(filename, FileLineInputDevice.save(filename, data).map(_ => Queue.empty), isBusy = false, 0L)
}
