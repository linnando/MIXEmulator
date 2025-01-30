package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.exceptions.UnsupportedPunchedCardCharacterException
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.CardReader

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FileCardReader(filename: String,
                          task: Future[Option[IndexedSeq[IOWord]]],
                          isBusy: Boolean,
                          pos: Long,
                          lowLevelOps: LineAccessFileInputOps)
  extends CardReader with FileLineInputDevice {

  override def blockSize: Int = CardReader.BLOCK_SIZE

  override protected def readLine(): Future[IndexedSeq[IOWord]] = {
    val eventualChars: Future[Array[Char]] = lowLevelOps.readLine(filename, pos)
    eventualChars.map(chars => {
      val sChars = chars.mkString.replace("0\u00af", "\u0394")
        .replace("1\u00af", "J")
        .replace("2\u00af", "K")
        .replace("3\u00af", "L")
        .replace("4\u00af", "M")
        .replace("5\u00af", "N")
        .replace("6\u00af", "O")
        .replace("7\u00af", "P")
        .replace("8\u00af", "Q")
        .replace("9\u00af", "R")
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

  override def withTask(task: Future[IndexedSeq[IOWord]]): FileCardReader =
    copy(task = task.map(Some(_)), isBusy = true, pos = pos + 1L)

  override def withoutTask: FileCardReader =
    copy(task = Future.successful(None), isBusy = false)

}

object FileCardReader {
  def create(filename: String, lowLevelOps: LineAccessFileInputOps): FileCardReader =
    FileCardReader(filename, lowLevelOps.initialise(filename).map(_ => None), isBusy = false, 0L, lowLevelOps)

  def create(filename: String, data: String, lowLevelOps: LineAccessFileInputOps): FileCardReader =
    FileCardReader(filename, lowLevelOps.save(filename, data).map(_ => None), isBusy = false, 0L, lowLevelOps)
}
