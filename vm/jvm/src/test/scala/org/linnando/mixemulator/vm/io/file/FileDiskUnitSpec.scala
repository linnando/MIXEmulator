package org.linnando.mixemulator.vm.io.file

import java.io._
import java.nio._

import org.linnando.mixemulator.vm.io.DiskUnit
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.FileMatchers
import org.specs2.mutable.Specification

class FileDiskUnitSpec(implicit ee: ExecutionEnv) extends Specification with FileMatchers {
  private val bytes0 = (0 until 500).map(i => (i % 64).toByte)
  private val words0 = IndexedSeq.tabulate(100)(i =>
    IOWord(negative = false, Seq.tabulate(5)(j => ((5 * i + j) % 64).toByte)))
  private val bytes1 = (0 until 100).flatMap(i =>
    ((5 * i % 64) | 0x80).toByte +: (1 until 5).map(j => ((5 * i + j) % 64).toByte))
  private val words1 = IndexedSeq.tabulate(100)(i =>
    IOWord(negative = true, Seq.tabulate(5)(j => ((5 * i + j) % 64).toByte)))

  "file emulator of a disk unit" should {
    "create a device with correct parameters" in {
      val filename = "disk0"
      val device = FileDiskUnit.create(filename)
      device.blockSize must be equalTo DiskUnit.BLOCK_SIZE
      device.filename must be equalTo filename
      device.version must be equalTo 0
      device.isBusy must beFalse
      new File(s"$filename/0").delete()
      new File(filename).delete()
    }

    "output data to a file" in {
      val filename = "disk1"
      val device = FileDiskUnit.create(filename)
      val busyState = device.write(0L, words0).write(1L, words1)
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(2).await
      finalState.map(_._1.isBusy) must beFalse.await
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

    "allow writing blocks in arbitrary order" in {
      val filename = "disk2"
      val device = FileDiskUnit.create(filename)
      val busyState = device.write(1L, words0).write(0L, words1)
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(2).await
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._2) must beNone.await
      val file1 = new File(s"$filename/1")
      file1 must exist
      val stream1 = new FileInputStream(file1).getChannel
      val buffer1 = ByteBuffer.allocate(1000)
      stream1.read(buffer1, 0)
      (0 until 500).map(buffer1.get) must be equalTo (0 until 500).map(_ => 0.toByte)
      (500 until 1000).map(buffer1.get) must be equalTo bytes0
      val file2 = new File(s"$filename/2")
      file2 must exist
      val stream2 = new FileInputStream(file2).getChannel
      val buffer2 = ByteBuffer.allocate(1000)
      stream2.read(buffer2, 0)
      (0 until 1000).map(buffer2.get) must be equalTo (bytes1 ++ bytes0)
      new File(s"$filename/0").delete()
      file1.delete()
      file2.delete()
      new File(filename).delete()
    }

    "read previously written data" in {
      val filename = "disk3"
      val device = FileDiskUnit.create(filename)
      val busyState = device.write(0L, words0).write(1L, words1).read(1L)
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(2).await
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._2) must beSome(words1).await
      new File(s"$filename/0").delete()
      new File(s"$filename/1").delete()
      new File(s"$filename/2").delete()
      new File(filename).delete()
    }

    "return zeroes on read beyond the end of file" in {
      val filename = "disk4"
      val directory = new File(filename)
      directory.mkdirs()
      val file = new File(s"$filename/0")
      val channel = new FileOutputStream(file).getChannel
      channel.close()
      val device = FileDiskUnit.create(filename)
      val busyState = device.read(0L)
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

    "do nothing on positioning" in {
      val filename = "disk5"
      val device = FileDiskUnit.create(filename)
      val nextState = device.positioned(1L)
      nextState.version must be equalTo 0
      nextState.isBusy must beFalse
      new File(s"$filename/0").delete()
      new File(filename).delete()
    }
  }
}
