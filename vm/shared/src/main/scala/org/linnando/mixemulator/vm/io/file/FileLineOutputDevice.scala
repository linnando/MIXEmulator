package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.{LineDevice, PositionalOutputDevice}
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FileLineOutputDevice extends LineDevice with PositionalOutputDevice {
  def filename: String

  def version: Int

  def tasks: Future[Unit]

  def write(words: IndexedSeq[IOWord]): FileLineOutputDevice = {
    val chars = new Array[Char](5 * blockSize)
    val buffer = mutable.ArrayBuffer.empty[Char]
    buffer ++= words.flatMap(_.toChars)
    buffer.copyToArray(chars)
    newVersion(tasks flatMap { _ => appendLine(chars) })
  }

  private def appendLine(chars: Array[Char]): Future[Unit] =
    LineAccessFile.appendLine(filename, version, chars)

  def newVersion(tasks: Future[Unit]): FileLineOutputDevice

  override def flush(): Future[(FileLineOutputDevice, Seq[IndexedSeq[IOWord]])] =
    tasks.map(_ => (withoutTasks, Seq.empty))

  def withoutTasks: FileLineOutputDevice

  override def data: Future[IndexedSeq[String]] =
    LineAccessFile.getData(filename, version).map((contents: String) => contents.split("\n"))
}

object FileLineOutputDevice {
  def initialise(filename: String): Future[Unit] =
    LineAccessFile.initialiseVersioned(filename)
}
