package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.exceptions.UnsupportedPunchedCardCharacterException
import org.linnando.mixemulator.vm.io.CardPunch

import scala.concurrent.Future

case class FileCardPunch(filename: String,
                         version: Int,
                         tasks: Future[Unit],
                         isBusy: Boolean)
  extends FileLineOutputDevice with CardPunch {

  override def blockSize: Int = CardPunch.BLOCK_SIZE

  override protected def appendLine(chars: Array[Char]): Future[Unit] = {
    if (chars.contains('\u03a3')) throw new UnsupportedPunchedCardCharacterException('\u03a3')
    if (chars.contains('\u03a0')) throw new UnsupportedPunchedCardCharacterException('\u03a0')
    if (chars.contains('=')) throw new UnsupportedPunchedCardCharacterException('=')
    if (chars.contains('$')) throw new UnsupportedPunchedCardCharacterException('$')
    if (chars.contains('<')) throw new UnsupportedPunchedCardCharacterException('<')
    if (chars.contains('>')) throw new UnsupportedPunchedCardCharacterException('>')
    if (chars.contains('@')) throw new UnsupportedPunchedCardCharacterException('@')
    if (chars.contains(';')) throw new UnsupportedPunchedCardCharacterException(';')
    if (chars.contains(':')) throw new UnsupportedPunchedCardCharacterException(':')
    if (chars.contains('\'')) throw new UnsupportedPunchedCardCharacterException('\'')
    LineAccessFile.appendLine(filename, version, chars)
  }

  override def newVersion(tasks: Future[Unit]): FileCardPunch =
    copy(version = version + 1, tasks = tasks, isBusy = true)

  override def withoutTasks: FileCardPunch =
    copy(tasks = Future.successful {}, isBusy = false)

}

object FileCardPunch {
  def create(filename: String): FileCardPunch =
    FileCardPunch(filename, 0, FileLineOutputDevice.initialise(filename), isBusy = false)
}
