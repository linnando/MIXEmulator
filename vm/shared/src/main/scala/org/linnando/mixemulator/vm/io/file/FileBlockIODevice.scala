package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.BlockDevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FileBlockIODevice extends BlockDevice {
  def filename: String

  def version: Int

  def tasks: Future[Queue[IndexedSeq[IOWord]]]

  protected def readBlock(pos: Long): Future[IndexedSeq[IOWord]] = {
    val eventualBytes: Future[Array[Byte]] = BlockAccessFile.readBlock(filename, version, 5 * pos * blockSize, 5 * blockSize)
    eventualBytes.map((bytes: Array[Byte]) => bytesToIOWords(bytes))
  }

  private def bytesToIOWords(bytes: Array[Byte]): IndexedSeq[IOWord] =
    (bytes.indices by 5).map(i => {
      val sign = (bytes(i) & 0x80) > 0
      val headByte = (bytes(i) & 0x7f).toByte
      IOWord(sign, headByte :: (i + 1 until i + 5).map(bytes).toList)
    })

  protected def writeBlock(pos: Long, words: IndexedSeq[IOWord]): Future[Unit] = {
    val bytes = words.flatMap(word => {
      val headByte = if (word.negative) (word.bytes.head | 0x80).toByte else word.bytes.head
      headByte :: (1 until 5).map(word.bytes).toList
    }).toArray
    BlockAccessFile.writeBlock(filename, version, 5 * pos * blockSize, bytes)
  }

  override def data: Future[IndexedSeq[IOWord]] = {
    val eventualBytes: Future[Array[Byte]] = BlockAccessFile.getData(filename, version)
    eventualBytes.map((bytes: Array[Byte]) => bytesToIOWords(bytes))
  }
}

object FileBlockIODevice {
  def initialise(filename: String): Future[Unit] =
    BlockAccessFile.initialiseWithCurrentVersion(filename)

  def save(filename: String, data: Array[Byte]): Future[Unit] =
    BlockAccessFile.save(filename, data)
}
