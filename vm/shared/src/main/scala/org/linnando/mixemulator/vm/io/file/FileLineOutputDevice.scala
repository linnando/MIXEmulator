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

  protected def appendLine(chars: Array[Char]): Future[Unit] =
    LineAccessFile.appendLine(filename, version, chars)

  def newVersion(tasks: Future[Unit]): FileLineOutputDevice

  override def flush(): Future[(FileLineOutputDevice, Option[IndexedSeq[IOWord]])] =
    tasks.map(_ => (withoutTasks, None))

  def withoutTasks: FileLineOutputDevice

  override def data: Future[IndexedSeq[String]] = for {
    _ <- tasks
    contents: String <- LineAccessFile.getData(filename, version)
  } yield contents.split("\n")
}

object FileLineOutputDevice {
  def initialise(filename: String): Future[Unit] =
    LineAccessFile.initialiseVersioned(filename)

  def getCurrentData(filename: String): Future[IndexedSeq[String]] = for {
    versions: Iterable[String] <- LineAccessFile.getVersions(filename)
    versionNumbers = versions.map(_.toInt)
    contents: String <- LineAccessFile.getData(filename, versionNumbers.max)
  } yield contents.split("\n")
}
