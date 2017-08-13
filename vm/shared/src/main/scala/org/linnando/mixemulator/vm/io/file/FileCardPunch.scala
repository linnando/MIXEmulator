package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.CardPunch

import scala.concurrent.Future

case class FileCardPunch(filename: String,
                         version: Int = 0,
                         tasks: Future[Unit] = Future.successful {},
                         isBusy: Boolean = false)
  extends FileLineOutputDevice with CardPunch {

  override def blockSize: Int = CardPunch.BLOCK_SIZE

  override def newVersion(tasks: Future[Unit]): FileCardPunch =
    copy(version = version + 1, tasks = tasks, isBusy = true)

  override def withoutTasks: FileCardPunch =
    copy(tasks = Future.successful {}, isBusy = false)

}
