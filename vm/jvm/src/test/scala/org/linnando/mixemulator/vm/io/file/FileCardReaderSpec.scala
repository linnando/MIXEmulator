package org.linnando.mixemulator.vm.io.file

import java.io.File

import org.linnando.mixemulator.vm.exceptions.EndOfFileException
import org.linnando.mixemulator.vm.io.CardReader
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.ContentMatchers
import org.specs2.mutable.Specification

import scala.collection.immutable.Queue

class FileCardReaderSpec(implicit ee: ExecutionEnv) extends Specification with ContentMatchers {
  val line0 = "01234567890123456789012345678901234567890123456789012345678901234567890123456789"
  val words0 = IndexedSeq(
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
  val words1 = IndexedSeq(
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

  "file emulator of a card reader" should {
    "create a device with correct parameters" in {
      val filename = "cardreader0"
      val device = FileCardReader.create(filename, "")
      device.blockSize must be equalTo CardReader.BLOCK_SIZE
      device.filename must be equalTo filename
      device.isBusy must beFalse
      device.pos must be equalTo 0L
      new File(filename).delete()
    }

    "input data from a file" in {
      val filename = "cardreader1"
      val file = new File(filename)
      val device = FileCardReader.create(filename, s"$line0\n$line1\n")
      val busyState = device.read().read()
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 2L
      val finalState = busyState.flush()
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(2L).await
      finalState.map(_._2) must beEqualTo(Queue(words0, words1)).await
      file must haveSameLinesAs(Seq(line0, line1))
      file.delete()
    }

    "throw an exception when the file end is reached" in {
      val filename = "cardreader2"
      val file = new File(filename)
      val device = FileCardReader.create(filename, "")
      val busyState = device.read()
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      busyState.flush() must throwAn[EndOfFileException].await
      file.delete()
    }
  }
}
