package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.{LineDevice, PositionalInputDevice}
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.ArraySeq
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
    val eventualChars: Future[Array[Char]] = lowLevelOps.readLine(filename, pos)
    eventualChars.map(chars =>
      (0 until blockSize).map(i =>
        IOWord((5 * i until 5 * (i + 1)).map(j => if (j < chars.length) chars(j) else ' '))
      )
    )
  }

  protected def lowLevelOps: LineAccessFileInputOps

  def withTask(task: Future[IndexedSeq[IOWord]]): FileLineInputDevice

  override def flush(): Future[(FileLineInputDevice, Option[IndexedSeq[IOWord]])] =
    task.map((withoutTask, _))

  def withoutTask: FileLineInputDevice

  override def data: Future[IndexedSeq[String]] = for {
    _ <- task
    contents: String <- lowLevelOps.getData(filename)
  } yield ArraySeq.unsafeWrapArray(contents.split("\n"))
}
