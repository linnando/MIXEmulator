package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.exceptions.UnsupportedPunchedCardCharacterException
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.CardReader

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FileCardReader(filename: String,
                          tasks: Future[Queue[IndexedSeq[IOWord]]],
                          isBusy: Boolean,
                          pos: Long)
  extends CardReader with FileLineInputDevice {

  override def blockSize: Int = CardReader.BLOCK_SIZE

  override protected def readLine(): Future[IndexedSeq[IOWord]] = {
    val eventualChars: Future[Array[Char]] = LineAccessFile.readLine(filename, pos)
    eventualChars.map(chars => {
      val sChars = chars.mkString.replaceAllLiterally("0\u00af", "\u0394")
        .replaceAllLiterally("1\u00af", "J")
        .replaceAllLiterally("2\u00af", "K")
        .replaceAllLiterally("3\u00af", "L")
        .replaceAllLiterally("4\u00af", "M")
        .replaceAllLiterally("5\u00af", "N")
        .replaceAllLiterally("6\u00af", "O")
        .replaceAllLiterally("7\u00af", "P")
        .replaceAllLiterally("8\u00af", "Q")
        .replaceAllLiterally("9\u00af", "R")
      if (sChars.contains('\u03a3')) throw new UnsupportedPunchedCardCharacterException('\u03a3')
      if (sChars.contains('\u03a0')) throw new UnsupportedPunchedCardCharacterException('\u03a0')
      if (sChars.contains('=')) throw new UnsupportedPunchedCardCharacterException('=')
      if (sChars.contains('$')) throw new UnsupportedPunchedCardCharacterException('$')
      if (sChars.contains('<')) throw new UnsupportedPunchedCardCharacterException('<')
      if (sChars.contains('>')) throw new UnsupportedPunchedCardCharacterException('>')
      if (sChars.contains('@')) throw new UnsupportedPunchedCardCharacterException('@')
      if (sChars.contains(';')) throw new UnsupportedPunchedCardCharacterException(';')
      if (sChars.contains(':')) throw new UnsupportedPunchedCardCharacterException(':')
      if (sChars.contains('\'')) throw new UnsupportedPunchedCardCharacterException('\'')
      (0 until blockSize).map(i => {
        IOWord((5 * i until 5 * (i + 1)).map(j => if (j < sChars.length) sChars(j) else ' '))
      })
    })
  }

  override def withTasks(tasks: Future[Queue[IndexedSeq[IOWord]]]): FileCardReader =
    copy(tasks = tasks, isBusy = true, pos = pos + 1L)

  override def withoutTasks: FileCardReader =
    copy(tasks = Future.successful(Queue.empty), isBusy = false)

}

object FileCardReader {
  def create(filename: String): FileCardReader =
    FileCardReader(filename, FileLineInputDevice.initialise(filename).map(_ => Queue.empty), isBusy = false, 0L)

  def create(filename: String, data: String): FileCardReader =
    FileCardReader(filename, FileLineInputDevice.save(filename, data).map(_ => Queue.empty), isBusy = false, 0L)
}
