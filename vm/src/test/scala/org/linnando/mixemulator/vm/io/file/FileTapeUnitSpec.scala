package org.linnando.mixemulator.vm.io.file

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import org.linnando.mixemulator.vm.io.TapeUnit
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.matcher.FileMatchers
import org.specs2.mutable.Specification

import scala.collection.immutable.Queue

class FileTapeUnitSpec extends Specification with FileMatchers {
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
      val device = FileTapeUnit(filename)
      device.blockSize must be equalTo TapeUnit.BLOCK_SIZE
      device.filename must be equalTo filename
      device.version must be equalTo 0
      device.tasks.isCompleted must beTrue
      device.isBusy must beFalse
      device.pos must be equalTo 0L
    }

    "output data to a file" in {
      val filename = "disk1"
      val device = FileTapeUnit(filename)
      val busyState = device.write(words0).write(words1)
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 2L
      val finalState = busyState.flush()
      finalState._1.version must be equalTo 2
      finalState._1.isBusy must beFalse
      finalState._1.pos must be equalTo 2L
      finalState._2 must beEmpty
      val file1 = new File(s"$filename.1")
      file1 must exist
      val stream1 = new FileInputStream(file1).getChannel
      val buffer1 = ByteBuffer.allocate(500)
      stream1.read(buffer1, 0)
      (0 until 500).map(buffer1.get) must be equalTo bytes0
      val file2 = new File(s"$filename.2")
      file2 must exist
      val stream2 = new FileInputStream(file2).getChannel
      val buffer2 = ByteBuffer.allocate(1000)
      stream2.read(buffer2, 0)
      (0 until 1000).map(buffer2.get) must be equalTo (bytes0 ++ bytes1)
      file1.delete()
      file2.delete()
    }

    "allow changing position in the file" in {
      val filename = "disk2"
      val device = FileTapeUnit(filename)
      val busyState = device.write(words0) match {
        case d: FileTapeUnit => d.positioned(-1L).write(words1)
      }
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      val finalState = busyState.flush()
      finalState._1.version must be equalTo 2
      finalState._1.isBusy must beFalse
      finalState._1.pos must be equalTo 1L
      finalState._2 must beEmpty
      val file1 = new File(s"$filename.1")
      file1 must exist
      val stream1 = new FileInputStream(file1).getChannel
      val buffer1 = ByteBuffer.allocate(1000)
      stream1.read(buffer1, 0)
      (0 until 500).map(buffer1.get) must be equalTo bytes0
      val file2 = new File(s"$filename.2")
      file2 must exist
      val stream2 = new FileInputStream(file2).getChannel
      val buffer2 = ByteBuffer.allocate(1000)
      stream2.read(buffer2, 0)
      (0 until 500).map(buffer2.get) must be equalTo bytes1
      file1.delete()
      file2.delete()
    }

    "read previously written data" in {
      val filename = "disk3"
      val device = FileTapeUnit(filename)
      val busyState = device.write(words0) match {
        case d: FileTapeUnit => d.positioned(-1L).read()
      }
      busyState.version must be equalTo 1
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      val finalState = busyState.flush()
      finalState._1.version must be equalTo 1
      finalState._1.isBusy must beFalse
      finalState._1.pos must be equalTo 1L
      finalState._2 must be equalTo Queue(words0)
      new File(s"$filename.1").delete()
    }

    "throw an exception on attempt to read beyond the end of file" in {
      val filename = "disk4"
      val file = new File(s"$filename.0")
      val channel = new FileOutputStream(file).getChannel
      channel.close()
      val device = FileTapeUnit(filename)
      val busyState = device.read()
      busyState.version must be equalTo 0
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState._1.version must be equalTo 0
      finalState._1.isBusy must beFalse
      finalState._2 must be equalTo Queue((0 until 100).map(_ => IOWord(negative = false, Seq(0, 0, 0, 0, 0))))
      file.delete()
    }
  }
}
