package org.linnando.mixemulator.vm.io.file

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import org.linnando.mixemulator.vm.io.TapeUnit
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.FileMatchers
import org.specs2.mutable.Specification

class FileTapeUnitSpec(implicit ee: ExecutionEnv) extends Specification with FileMatchers {
  private val lowLevelOps: BlockAccessFileOps = BlockAccessFileOps.create()

  private val bytes0 = (0 until 500).map(i => (i % 64).toByte)
  private val words0 = IndexedSeq.tabulate(100)(i =>
    IOWord(negative = false, Seq.tabulate(5)(j => ((5 * i + j) % 64).toByte)))
  private val bytes1 = (0 until 100).flatMap(i =>
    ((5 * i % 64) | 0x80).toByte +: (1 until 5).map(j => ((5 * i + j) % 64).toByte))
  private val words1 = IndexedSeq.tabulate(100)(i =>
    IOWord(negative = true, Seq.tabulate(5)(j => ((5 * i + j) % 64).toByte)))

  "file emulator of a tape unit" should {
    "create a device with correct parameters" in {
      val filename = "tape0"
      val device = FileTapeUnit.create(filename, lowLevelOps)
      device.blockSize must be equalTo TapeUnit.BLOCK_SIZE
      device.filename must be equalTo filename
      device.version must be equalTo 0
      device.isBusy must beFalse
      device.pos must be equalTo 0L
      new File(s"$filename/0").delete()
      new File(filename).delete()
    }

    "output data to a file" in {
      val filename = "tape1"
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1)
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 2L
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(2).await
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(2L).await
      finalState.map(_._2) must beNone.await
      val file1 = new File(s"$filename/1")
      file1 must exist
      val stream1 = new FileInputStream(file1).getChannel
      val buffer1 = ByteBuffer.allocate(500)
      stream1.read(buffer1, 0)
      (0 until 500).map(buffer1.get) must be equalTo bytes0
      val file2 = new File(s"$filename/2")
      file2 must exist
      val stream2 = new FileInputStream(file2).getChannel
      val buffer2 = ByteBuffer.allocate(1000)
      stream2.read(buffer2, 0)
      (0 until 1000).map(buffer2.get) must be equalTo (bytes0 ++ bytes1)
      new File(s"$filename/0").delete()
      file1.delete()
      file2.delete()
      new File(filename).delete()
    }

    "allow changing position in the file" in {
      val filename = "tape2"
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1) match {
        case d: FileTapeUnit => d.positioned(-1L).write(words0)
      }
      busyState.version must be equalTo 3
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 2L
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(3).await
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(2L).await
      finalState.map(_._2) must beNone.await
      val file1 = new File(s"$filename/1")
      file1 must exist
      val stream1 = new FileInputStream(file1).getChannel
      val buffer1 = ByteBuffer.allocate(500)
      stream1.read(buffer1, 0)
      (0 until 500).map(buffer1.get) must be equalTo bytes0
      val file2 = new File(s"$filename/2")
      file2 must exist
      val stream2 = new FileInputStream(file2).getChannel
      val buffer2 = ByteBuffer.allocate(1000)
      stream2.read(buffer2, 0)
      (0 until 500).map(buffer2.get) must be equalTo bytes0
      (500 until 1000).map(buffer2.get) must be equalTo bytes1
      val file3 = new File(s"$filename/3")
      file3 must exist
      val stream3 = new FileInputStream(file3).getChannel
      val buffer3 = ByteBuffer.allocate(1000)
      stream3.read(buffer3, 0)
      (0 until 500).map(buffer3.get) must be equalTo bytes0
      (500 until 1000).map(buffer3.get) must be equalTo bytes0
      new File(s"$filename/0").delete()
      file1.delete()
      file2.delete()
      file3.delete()
      new File(filename).delete()
    }

    "rewind to the beginning when position is zero" in {
      val filename = "tape3"
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1) match {
        case d: FileTapeUnit => d.positioned(0).write(words1)
      }
      busyState.version must be equalTo 3
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(3).await
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(1L).await
      finalState.map(_._2) must beNone.await
      val file1 = new File(s"$filename/1")
      file1 must exist
      val stream1 = new FileInputStream(file1).getChannel
      val buffer1 = ByteBuffer.allocate(500)
      stream1.read(buffer1, 0)
      (0 until 500).map(buffer1.get) must be equalTo bytes0
      val file2 = new File(s"$filename/2")
      file2 must exist
      val stream2 = new FileInputStream(file2).getChannel
      val buffer2 = ByteBuffer.allocate(1000)
      stream2.read(buffer2, 0)
      (0 until 500).map(buffer2.get) must be equalTo bytes0
      (500 until 1000).map(buffer2.get) must be equalTo bytes1
      val file3 = new File(s"$filename/3")
      file3 must exist
      val stream3 = new FileInputStream(file3).getChannel
      val buffer3 = ByteBuffer.allocate(1000)
      stream3.read(buffer3, 0)
      (0 until 500).map(buffer3.get) must be equalTo bytes1
      (500 until 1000).map(buffer3.get) must be equalTo bytes1
      new File(s"$filename/0").delete()
      file1.delete()
      file2.delete()
      file3.delete()
      new File(filename).delete()
    }

    "read previously written data" in {
      val filename = "tape4"
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0) match {
        case d: FileTapeUnit => d.positioned(-1L).read()
      }
      busyState.version must be equalTo 1
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(1).await
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(1L).await
      finalState.map(_._2) must beSome(words0).await
      new File(s"$filename/0").delete()
      new File(s"$filename/1").delete()
      new File(filename).delete()
    }

    "read zeroes on read beyond the end of file" in {
      val filename = "tape5"
      val directory = new File(filename)
      directory.mkdirs()
      val file = new File(s"$filename/0")
      val channel = new FileOutputStream(file).getChannel
      channel.close()
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.read()
      busyState.version must be equalTo 0
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(0).await
      finalState.map(_._1.isBusy) must beFalse.await
      val expected: IndexedSeq[IOWord] = (0 until 100).map(_ => IOWord(negative = false, Seq(0, 0, 0, 0, 0)))
      finalState.map(_._2) must beSome(expected).await
      file.delete()
      directory.delete()
    }
  }
}
