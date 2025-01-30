package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.exceptions.EndOfFileException
import org.linnando.mixemulator.vm.io.PaperTape
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.FutureOutcome
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import java.io.File
import scala.concurrent.ExecutionContext

class FilePaperTapeSpec extends AsyncWordSpec with Matchers {
  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  private val lowLevelOps: LineAccessFileInputOps = LineAccessFileInputOps.create()
  val filename = "papertape"

  val line0 = "0123456789012345678901234567890123456789012345678901234567890123456789"
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
    IOWord(Seq('5', '6', '7', '8', '9'))
  )

  val line1 = "5678901234567890123456789012345678901234567890123456789012345678901234"
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
    IOWord(Seq('0', '1', '2', '3', '4'))
  )

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    complete {
      super.withFixture(test)
    } lastly {
      new File(filename).delete()
    }
  }

  "file emulator of a paper tape" should {
    "create a device with correct parameters" in {
      val device = FilePaperTape.create(filename, "", lowLevelOps)
      device.blockSize mustEqual PaperTape.BLOCK_SIZE
      device.filename mustEqual filename
      device.isBusy mustEqual false
      device.pos mustEqual 0L
    }

    "input data from a file" in {
      val device = FilePaperTape.create(filename, s"$line0\n$line1\n", lowLevelOps)
      val busyState = device.read()
      busyState.isBusy mustEqual true
      busyState.pos mustEqual 1L
      busyState.flush().map(finalState => {
        finalState._1.isBusy mustEqual false
        finalState._1.pos mustEqual 1L
        finalState._2 must contain(words0)
      })
    }

    "input multiple lines from a file" in {
      val device = FilePaperTape.create(filename, s"$line0\n$line1\n", lowLevelOps)
      val eventualBusyState = for {
        firstRead <- device.read().flush()
      } yield firstRead._1.read()
      eventualBusyState.flatMap(busyState => {
        busyState.isBusy mustEqual true
        busyState.pos mustEqual 2L
        busyState.flush().map(finalState => {
          finalState._1.isBusy mustEqual false
          finalState._1.pos mustEqual 2L
          finalState._2 must contain(words1)
        })
      })
    }

    "throw an exception when the file end is reached" in {
      val device = FilePaperTape.create(filename, "", lowLevelOps)
      val busyState = device.read()
      busyState.isBusy mustEqual true
      busyState.pos mustEqual 1L
      recoverToSucceededIf[EndOfFileException] {
        busyState.flush()
      }
    }

    "reset the reading position" in {
      val device = FilePaperTape.create(filename, s"$line0\n", lowLevelOps)
      val eventualBusyState = for {
        firstRead <- device.read().flush()
      } yield firstRead._1 match {
        case d: FilePaperTape => d.reset().read()
      }
      eventualBusyState.flatMap(busyState => {
        busyState.isBusy mustEqual true
        busyState.pos mustEqual 1L
        busyState.flush().map(finalState => {
          finalState._1.isBusy mustEqual false
          finalState._1.pos mustEqual 1L
          finalState._2 must contain(words0)
        })
      })
    }
  }
}
