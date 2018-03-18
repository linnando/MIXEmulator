package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.{LineDevice, PositionalInputDevice}
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FileLineInputDevice extends LineDevice with PositionalInputDevice {
  def filename: String

  def pos: Long

  def task: Future[Option[IndexedSeq[IOWord]]]

  def read(): FileLineInputDevice = withTask(task flatMap {
    case None => readLine()
    case Some(_) => throw new Error
  })

  protected def readLine(): Future[IndexedSeq[IOWord]] = {
    val eventualChars: Future[Array[Char]] = LineAccessFile.readLine(filename, pos)
    eventualChars.map(chars =>
      (0 until blockSize).map(i =>
        IOWord((5 * i until 5 * (i + 1)).map(j => if (j < chars.length) chars(j) else ' '))
      )
    )
  }

  def withTask(task: Future[IndexedSeq[IOWord]]): FileLineInputDevice

  override def flush(): Future[(FileLineInputDevice, Option[IndexedSeq[IOWord]])] =
    task.map((withoutTask, _))

  def withoutTask: FileLineInputDevice

  override def data: Future[IndexedSeq[String]] =
    LineAccessFile.getData(filename).map((contents: String) => contents.split("\n"))
}

object FileLineInputDevice {
  def initialise(filename: String): Future[Unit] =
    LineAccessFile.initialiseNonVersioned(filename)

  def save(filename: String, data: String): Future[Unit] =
    LineAccessFile.saveNonVersioned(filename, data)
}