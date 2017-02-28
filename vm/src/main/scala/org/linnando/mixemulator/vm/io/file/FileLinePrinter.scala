package org.linnando.mixemulator.vm.io.file

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import java.util.function.Consumer

import org.linnando.mixemulator.vm.io.LinePrinter

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

case class FileLinePrinter(filename: String, version: Int, tasks: Future[Unit], isBusy: Boolean)
  extends LinePrinter with FileLineOutputDevice {

  override def blockSize: Int = LinePrinter.BLOCK_SIZE

  override def newVersion(tasks: Future[Unit]): FileLinePrinter =
    copy(version = version + 1, tasks = tasks, isBusy = true)

  override def withoutTasks: FileLinePrinter =
    copy(tasks = Future.successful {}, isBusy = false)

  override def newPage(): FileLinePrinter =
    newVersion(tasks andThen { case Success(_) => writeNewPage() })

  def writeNewPage(): Unit = {
    val oldFile = new BufferedReader(new FileReader(s"$filename.$version"))
    val file = new BufferedWriter(new FileWriter(s"$filename.${version + 1}", true))
    try {
      oldFile.lines().forEach(new Consumer[String] {
        override def accept(s: String): Unit = file.write(s)
      })
      file.write("\f")
    }
    finally {
      file.close()
    }
  }

}
