package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.SequentialIODevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success

trait FileSequentialBlockIODevice extends SequentialIODevice with FileBlockIODevice {
  def pos: Long
  def tasks: Future[Queue[IndexedSeq[IOWord]]]

  override def read(): FileSequentialBlockIODevice =
    withTasks(tasks flatMap { prev => Future { prev.enqueue(readBlock(pos)) }})

  def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileSequentialBlockIODevice

  override def write(words: IndexedSeq[IOWord]): FileSequentialBlockIODevice =
    newVersion(tasks andThen { case Success(_) => writeBlock(pos, words) })

  def newVersion(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileSequentialBlockIODevice

  override def flush(): (FileSequentialBlockIODevice, Queue[IndexedSeq[IOWord]]) =
    (withoutTasks, Await.result(tasks, 1 seconds))

  def withoutTasks: FileSequentialBlockIODevice

}
