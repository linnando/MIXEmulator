package org.linnando.mixemulator.vm.io.file

import java.io._

import org.linnando.mixemulator.vm.io.LinePrinter

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

case class FileLinePrinter(filename: String,
                           version: Int = 0,
                           tasks: Future[Unit] = Future.successful {},
                           isBusy: Boolean = false)
  extends LinePrinter with FileLineOutputDevice {

  override def blockSize: Int = LinePrinter.BLOCK_SIZE

  override def newVersion(tasks: Future[Unit]): FileLinePrinter =
    copy(version = version + 1, tasks = tasks, isBusy = true)

  override def withoutTasks: FileLinePrinter =
    copy(tasks = Future.successful {}, isBusy = false)

  override def newPage(): FileLinePrinter =
    newVersion(tasks andThen { case Success(_) => writeNewPage() })

  def writeNewPage(): Unit = {
    val oldFile = new File(s"$filename.$version")
    val writer = new BufferedWriter(new FileWriter(s"$filename.${version + 1}"))
    try {
      if (oldFile.exists)
        copyFileContent(oldFile, writer)
      writer.write("\f")
    }
    finally {
      writer.close()
    }
  }

}
