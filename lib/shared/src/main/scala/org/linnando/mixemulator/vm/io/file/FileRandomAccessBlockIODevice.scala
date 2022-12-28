package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.RandomAccessIODevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FileRandomAccessBlockIODevice extends RandomAccessIODevice with FileBlockIODevice {
  override def read(pos: Long): FileRandomAccessBlockIODevice = withTask(task flatMap {
      case None => readBlock(pos).map(words => Some(words))
      case Some(_) => throw new Error
    })

  def withTask(task: Future[Option[IndexedSeq[IOWord]]]): FileRandomAccessBlockIODevice

  override def write(pos: Long, words: IndexedSeq[IOWord]): FileRandomAccessBlockIODevice =
    newVersion(task flatMap { prev => writeBlock(pos, words).map(_ => prev) })

  def newVersion(tasks: Future[Option[IndexedSeq[IOWord]]]): FileRandomAccessBlockIODevice

  override def flush(): Future[(FileRandomAccessBlockIODevice, Option[IndexedSeq[IOWord]])] =
    task.map((withoutTask, _))

  def withoutTask: FileRandomAccessBlockIODevice
}
