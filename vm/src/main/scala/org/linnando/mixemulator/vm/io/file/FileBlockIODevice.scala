package org.linnando.mixemulator.vm.io.file

import java.io.{FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

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
    val oldFile = new FileInputStream(s"$filename.$version").getChannel
    val file = new FileOutputStream(s"$filename.${version + 1}").getChannel
    try {
      oldFile.transferTo(0, 5 * pos * blockSize, file)
      file.write(buffer)
      oldFile.transferTo(5 * (pos + 1) * blockSize, Long.MaxValue, file)
    }
    finally {
      oldFile.close()
      file.close()
    }
  }
}
