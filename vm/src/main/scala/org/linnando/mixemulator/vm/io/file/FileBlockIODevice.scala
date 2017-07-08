package org.linnando.mixemulator.vm.io.file

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import org.linnando.mixemulator.vm.io.Device
import org.linnando.mixemulator.vm.io.data.IOWord

trait FileBlockIODevice extends Device {
  def filename: String
  def version: Int

  protected def readBlock(pos: Long): IndexedSeq[IOWord] = {
    val buffer = ByteBuffer.allocate(5 * blockSize)
    val file = new FileInputStream(s"$filename.$version").getChannel
    try {
      file.read(buffer, 5 * pos * blockSize)
      IndexedSeq.range(0, 5 * blockSize, 5) map { i =>
        IOWord((buffer.get(i) & 0x80) > 0, (buffer.get(i) & 0x7f).toByte :: (i + 1 until i + 5).map(buffer.get).toList)
      }
    }
    finally {
      file.close()
    }
  }

  protected def writeBlock(pos: Long, words: IndexedSeq[IOWord]): Unit = {
    val buffer = ByteBuffer.allocate(5 * blockSize)
    words.zipWithIndex foreach { w =>
      val i = 5 * w._2
      val bytes = w._1.bytes
      buffer.put(i, if (w._1.negative) (bytes.head | 0x80).toByte else bytes.head)
      (1 until 5) foreach { j => buffer.put(i + j, bytes(j)) }
    }
    val oldFile = new File(s"$filename.$version")
    val channel = new FileOutputStream(s"$filename.${version + 1}").getChannel
    try {
      if (oldFile.exists)
        copyFileBeginning(oldFile, channel, pos)
      else
        fillZeroes(channel, pos)
      channel.write(buffer)
      if (oldFile.exists)
        copyFileEnd(oldFile, channel, pos)
    }
    finally {
      channel.close()
    }
  }

  private def copyFileBeginning(source: File, destChannel: FileChannel, pos: Long) = {
    val srcChannel = new FileInputStream(source).getChannel
    srcChannel.transferTo(0, 5 * pos * blockSize, destChannel)
    srcChannel.close()
  }

  private def fillZeroes(destChannel: FileChannel, pos: Long) = {
    val buffer = ByteBuffer.allocate(5 * blockSize)
    (0 until 5 * blockSize) foreach { i => buffer.put(i, 0) }
    (0 until pos.toInt) foreach { _ => destChannel.write(buffer) }
  }

  private def copyFileEnd(source: File, destChannel: FileChannel, pos: Long) = {
    val srcChannel = new FileInputStream(source).getChannel
    srcChannel.transferTo(5 * (pos + 1) * blockSize, Long.MaxValue, destChannel)
    srcChannel.close()
  }
}
