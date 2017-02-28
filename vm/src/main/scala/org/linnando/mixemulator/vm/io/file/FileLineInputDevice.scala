package org.linnando.mixemulator.vm.io.file

import java.io.{BufferedReader, FileReader}

import org.linnando.mixemulator.vm.io.PositionalInputDevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

trait FileLineInputDevice extends PositionalInputDevice {
  def filename: String

  def pos: Long

  def tasks: Future[Queue[IndexedSeq[IOWord]]]

  def read(): FileLineInputDevice =
    withTasks(tasks flatMap { prev => Future { prev.enqueue(readLine()) }})

  def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileLineInputDevice

  private def readLine(): IndexedSeq[IOWord] = {
    val file = new BufferedReader(new FileReader(filename))
    try {
      file.skip((5 * blockSize + 1) * pos)
      val chars = file.readLine().toCharArray
      IndexedSeq.range(0, 5 * blockSize, 5) map { i => IOWord((i until i + 5).map(chars)) }
    }
    finally {
      file.close()
    }
  }

  override def flush(): (FileLineInputDevice, Queue[IndexedSeq[IOWord]]) =
    (withoutTasks, Await.result(tasks, 1 seconds))

  def withoutTasks: FileLineInputDevice
}
