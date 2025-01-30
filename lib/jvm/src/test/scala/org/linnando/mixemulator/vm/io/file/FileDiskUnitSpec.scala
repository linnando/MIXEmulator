package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.DiskUnit
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.FutureOutcome
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import java.io._
import scala.concurrent.ExecutionContext

class FileDiskUnitSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  private val lowLevelOps: BlockAccessFileOps = BlockAccessFileOps.create()
  val filename = "disk"

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

  "file emulator of a disk unit" should {
    "create a device with correct parameters" in {
      val device = FileDiskUnit.create(filename, lowLevelOps)
      device.blockSize mustEqual DiskUnit.BLOCK_SIZE
      device.filename mustEqual filename
      device.version mustEqual 0
      device.isBusy mustEqual false
    }

    "output data to a file" in {
      val device = FileDiskUnit.create(filename, lowLevelOps)
      val busyState = device.write(0L, words0).write(1L, words1)
      busyState.version mustEqual 2
      busyState.isBusy mustEqual true
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 2
        finalState._1.isBusy mustEqual false
        finalState._2 mustBe empty
        new File(s"$filename/1") must exist
        new File(s"$filename/2") must exist
      })
    }

    "allow writing blocks in arbitrary order" in {
      val device = FileDiskUnit.create(filename, lowLevelOps)
      val busyState = device.write(1L, words0).write(0L, words1)
      busyState.version mustEqual 2
      busyState.isBusy mustEqual true
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 2
        finalState._1.isBusy mustEqual false
        finalState._2 mustBe empty
        new File(s"$filename/1") must exist
        new File(s"$filename/2") must exist
      })
    }

    "read previously written data" in {
      val device = FileDiskUnit.create(filename, lowLevelOps)
      val busyState = device.write(0L, words0).write(1L, words1).read(1L)
      busyState.version mustEqual 2
      busyState.isBusy mustEqual true
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 2
        finalState._1.isBusy mustEqual false
        finalState._2 must contain(words1)
      })
    }

    "return zeroes on read beyond the end of file" in {
      val directory = new File(filename)
      directory.mkdirs()
      val file = new File(s"$filename/0")
      val channel = new FileOutputStream(file).getChannel
      channel.close()
      val device = FileDiskUnit.create(filename, lowLevelOps)
      val busyState = device.read(0L)
      busyState.version mustEqual 0
      busyState.isBusy mustEqual true
      busyState.flush().map(finalState => {
        finalState._1.version mustEqual 0
        finalState._1.isBusy mustEqual false
        finalState._2 must contain((0 until 100).map(_ => IOWord(negative = false, Seq(0, 0, 0, 0, 0))))
      })
    }

    "do nothing on positioning" in {
      val device = FileDiskUnit.create(filename, lowLevelOps)
      val nextState = device.positioned(1L)
      nextState.version mustEqual 0
      nextState.isBusy mustEqual false
    }
  }
}
