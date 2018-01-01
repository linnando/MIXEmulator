package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.{LineDevice, PositionalInputDevice}
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FileLineInputDevice extends LineDevice with PositionalInputDevice {
  def filename: String

  def pos: Long

  def tasks: Future[Queue[IndexedSeq[IOWord]]]

  def read(): FileLineInputDevice = withTasks(tasks flatMap { prev =>
    readLine().map(words => prev.enqueue(words))
  })

  private def readLine(): Future[IndexedSeq[IOWord]] = {
    val eventualChars: Future[Array[Char]] = LineAccessFile.readLine(filename, pos * (5 * blockSize + 1))
    eventualChars.map(chars => (0 until 5 * blockSize by 5).map(i => IOWord((i until i + 5).map(chars))))
  }

  def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileLineInputDevice

  override def flush(): Future[(FileLineInputDevice, Queue[IndexedSeq[IOWord]])] =
    tasks.map((withoutTasks, _))

  def withoutTasks: FileLineInputDevice

  override def data: Future[IndexedSeq[String]] =
    LineAccessFile.getData(filename).map((contents: String) => contents.split("\n"))
}

object FileLineInputDevice {
  def initialise(filename: String): Future[Unit] =
    LineAccessFile.initialiseNonVersioned(filename)

  def save(filename: String, data: String): Future[Unit] =
    LineAccessFile.saveNonVersioned(filename, data)
}