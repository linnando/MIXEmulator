package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.SequentialIODevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FileSequentialBlockIODevice extends SequentialIODevice with FileBlockIODevice {
  def pos: Long

  override def read(): FileSequentialBlockIODevice =
    withTasks(task flatMap {
      case None => readBlock(pos).map(words => Some(words))
      case Some(_) => throw new Error
    })

  def withTasks(tasks: Future[Option[IndexedSeq[IOWord]]]): FileSequentialBlockIODevice

  override def write(words: IndexedSeq[IOWord]): FileSequentialBlockIODevice =
    newVersion(task flatMap { prev => writeBlock(pos, words).map(_ => prev) })

  def newVersion(tasks: Future[Option[IndexedSeq[IOWord]]]): FileSequentialBlockIODevice

  override def flush(): Future[(FileSequentialBlockIODevice, Option[IndexedSeq[IOWord]])] =
    task.map((withoutTasks, _))

  def withoutTasks: FileSequentialBlockIODevice

}
