package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.SequentialIODevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait FileSequentialBlockIODevice extends SequentialIODevice with FileBlockIODevice {
  def pos: Long

  override def read(): FileSequentialBlockIODevice =
    withTasks(tasks flatMap { prev => readBlock(pos).map(words => prev.enqueue(words)) })

  def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileSequentialBlockIODevice

  override def write(words: IndexedSeq[IOWord]): FileSequentialBlockIODevice =
    newVersion(tasks flatMap { prev => writeBlock(pos, words).map(_ => prev) })

  def newVersion(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileSequentialBlockIODevice

  override def flush(): Future[(FileSequentialBlockIODevice, Queue[IndexedSeq[IOWord]])] =
    tasks.map((withoutTasks, _))

  def withoutTasks: FileSequentialBlockIODevice

}
