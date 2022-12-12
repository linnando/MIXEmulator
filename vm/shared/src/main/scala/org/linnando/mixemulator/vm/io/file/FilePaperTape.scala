package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.PaperTape
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FilePaperTape(filename: String,
                         task: Future[Option[IndexedSeq[IOWord]]],
                         isBusy: Boolean,
                         pos: Long,
                         lowLevelOps: LineAccessFileInputOps)
  extends PaperTape with FileLineInputDevice {

  override def blockSize: Int = PaperTape.BLOCK_SIZE

  override def withTask(task: Future[IndexedSeq[IOWord]]): FilePaperTape =
    copy(task = task.map(Some(_)), isBusy = true, pos = pos + 1L)

  override def withoutTask: FilePaperTape =
    copy(task = Future.successful(None), isBusy = false)

  override def reset(): FilePaperTape = copy(pos = 0L)
}

object FilePaperTape {
  def create(filename: String, lowLevelOps: LineAccessFileInputOps): FilePaperTape =
    FilePaperTape(filename, lowLevelOps.initialise(filename).map(_ => None), isBusy = false, 0L, lowLevelOps)

  def create(filename: String, data: String, lowLevelOps: LineAccessFileInputOps): FilePaperTape =
    FilePaperTape(filename, lowLevelOps.save(filename, data).map(_ => None), isBusy = false, 0L, lowLevelOps)
}
