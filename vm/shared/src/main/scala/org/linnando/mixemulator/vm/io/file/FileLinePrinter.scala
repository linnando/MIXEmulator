package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.LinePrinter

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class FileLinePrinter(filename: String,
                           version: Int,
                           tasks: Future[Unit],
                           isBusy: Boolean)
  extends LinePrinter with FileLineOutputDevice {

  override def blockSize: Int = LinePrinter.BLOCK_SIZE

  override def newVersion(tasks: Future[Unit]): FileLinePrinter =
    copy(version = version + 1, tasks = tasks, isBusy = true)

  override def withoutTasks: FileLinePrinter =
    copy(tasks = Future.successful {}, isBusy = false)

  override def newPage(): FileLinePrinter =
    newVersion(tasks flatMap { _ => appendNewPage() })

  private def appendNewPage(): Future[Unit] =
    LineAccessFile.appendNewPage(filename, version)
}

object FileLinePrinter {
  def create(filename: String): FileLinePrinter =
    FileLinePrinter(filename, 0, FileLineOutputDevice.initialise(filename), isBusy = false)
}
