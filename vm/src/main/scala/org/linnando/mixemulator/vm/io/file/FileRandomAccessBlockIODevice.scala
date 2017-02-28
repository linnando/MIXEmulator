package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.RandomAccessIODevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success

trait FileRandomAccessBlockIODevice extends RandomAccessIODevice with FileBlockIODevice {
  def tasks: Future[Queue[IndexedSeq[IOWord]]]

  override def read(pos: Long): FileRandomAccessBlockIODevice =
    withTasks(tasks flatMap { prev => Future { prev.enqueue(readBlock(pos)) }})

  def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileRandomAccessBlockIODevice

  override def write(pos: Long, words: IndexedSeq[IOWord]): FileRandomAccessBlockIODevice =
    newVersion(tasks andThen { case Success(_) => writeBlock(pos, words) })

  def newVersion(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileRandomAccessBlockIODevice

  override def flush(): (FileRandomAccessBlockIODevice, Queue[IndexedSeq[IOWord]]) =
    (withoutTasks, Await.result(tasks, 1 seconds))

  def withoutTasks: FileRandomAccessBlockIODevice
}
