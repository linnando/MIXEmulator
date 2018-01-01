package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.RandomAccessIODevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FileRandomAccessBlockIODevice extends RandomAccessIODevice with FileBlockIODevice {
  override def read(pos: Long): FileRandomAccessBlockIODevice =
    withTasks(tasks flatMap { prev => readBlock(pos).map(words => prev.enqueue(words)) })

  def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileRandomAccessBlockIODevice

  override def write(pos: Long, words: IndexedSeq[IOWord]): FileRandomAccessBlockIODevice =
    newVersion(tasks flatMap { prev => writeBlock(pos, words).map(_ => prev) })

  def newVersion(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileRandomAccessBlockIODevice

  override def flush(): Future[(FileRandomAccessBlockIODevice, Queue[IndexedSeq[IOWord]])] =
    tasks.map((withoutTasks, _))

  def withoutTasks: FileRandomAccessBlockIODevice
}
