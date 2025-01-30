package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.TapeUnit
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.FutureOutcome
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import java.io.{File, FileOutputStream}
import scala.concurrent.ExecutionContext

class FileTapeUnitSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  private val lowLevelOps: BlockAccessFileOps = BlockAccessFileOps.create()
  val filename = "tapeunit"

  private val words0 = IndexedSeq.tabulate(100)(i =>
    IOWord(negative = false, Seq.tabulate(5)(j => ((5 * i + j) % 64).toByte)))
  private val words1 = IndexedSeq.tabulate(100)(i =>
    IOWord(negative = true, Seq.tabulate(5)(j => ((5 * i + j) % 64).toByte)))

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    complete {
      super.withFixture(test)
    } lastly {
      val dir = new File(filename)
      val files = dir.listFiles()
      if (files != null) {
        files.foreach(file => {
          file.delete()
        })
      }
      dir.delete()
    }
  }

  "file emulator of a tape unit" should {
    "create a device with correct parameters" in {
      val device = FileTapeUnit.create(filename, lowLevelOps)
      device.blockSize mustEqual TapeUnit.BLOCK_SIZE
      device.filename mustEqual filename
      device.version mustEqual 0
      device.isBusy mustEqual false
      device.pos mustEqual 0L
    }

    "output data to a file" in {
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1)
      busyState.version mustEqual 2
      busyState.isBusy mustEqual true
      busyState.pos mustEqual 2L
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 2
        finalState._1.isBusy mustEqual false
        finalState._1.pos mustEqual 2L
        finalState._2 mustBe empty
        new File(s"$filename/1") must exist
        new File(s"$filename/2") must exist
      })
    }

    "allow changing position in the file" in {
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1) match {
        case d: FileTapeUnit => d.positioned(-1L).write(words0)
      }
      busyState.version mustEqual 3
      busyState.isBusy mustEqual true
      busyState.pos mustEqual 2L
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 3
        finalState._1.isBusy mustEqual false
        finalState._1.pos mustEqual 2L
        finalState._2 mustBe empty
        new File(s"$filename/1") must exist
        new File(s"$filename/2") must exist
        new File(s"$filename/3") must exist
      })
    }

    "rewind to the beginning when position is zero" in {
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1) match {
        case d: FileTapeUnit => d.positioned(0).write(words1)
      }
      busyState.version mustEqual 3
      busyState.isBusy mustEqual true
      busyState.pos mustEqual 1L
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 3
        finalState._1.isBusy mustEqual false
        finalState._1.pos mustEqual 1L
        finalState._2 mustBe empty
        new File(s"$filename/1") must exist
        new File(s"$filename/2") must exist
        new File(s"$filename/3") must exist
      })
    }

    "read previously written data" in {
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.write(words0) match {
        case d: FileTapeUnit => d.positioned(-1L).read()
      }
      busyState.version mustEqual 1
      busyState.isBusy mustEqual true
      busyState.pos mustEqual 1L
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 1
        finalState._1.isBusy mustEqual false
        finalState._1.pos mustEqual 1L
        finalState._2 must contain(words0)
      })
    }

    "read zeroes on read beyond the end of file" in {
      val directory = new File(filename)
      directory.mkdirs()
      val file = new File(s"$filename/0")
      val channel = new FileOutputStream(file).getChannel
      channel.close()
      val device = FileTapeUnit.create(filename, lowLevelOps)
      val busyState = device.read()
      busyState.version mustEqual 0
      busyState.isBusy mustEqual true
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 0
        finalState._1.isBusy mustEqual false
        finalState._2 must contain((0 until 100).map(_ => IOWord(negative = false, Seq(0, 0, 0, 0, 0))))
      })
    }
  }
}
