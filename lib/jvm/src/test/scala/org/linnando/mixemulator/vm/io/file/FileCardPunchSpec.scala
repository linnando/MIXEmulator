package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.io.CardPunch
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.FutureOutcome
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import java.io.File
import scala.concurrent.ExecutionContext

class FileCardPunchSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  private val lowLevelOps: LineAccessFileOutputOps = LineAccessFileOutputOps.create()
  val filename = "cardpunch"

  val line0 = "01234567890123456789012345678901234567890123456789012345678901234567890123456789"
  val words0: IndexedSeq[IOWord] = IndexedSeq(
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9'))
  )

  val line1 = "56789012345678901234567890123456789012345678901234567890123456789012345678901234"
  val words1: IndexedSeq[IOWord] = IndexedSeq(
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4')),
    IOWord(Seq('5', '6', '7', '8', '9')),
    IOWord(Seq('0', '1', '2', '3', '4'))
  )

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

  "file emulator of a card punch" should {
    "create a device with correct parameters" in {
      val device = FileCardPunch.create(filename, lowLevelOps)
      device.blockSize mustEqual CardPunch.BLOCK_SIZE
      device.filename mustEqual filename
      device.version mustEqual 0
      device.isBusy mustEqual false
    }

    "output data to a file" in {
      val device = FileCardPunch.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1)
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
  }
}
