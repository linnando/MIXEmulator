package org.linnando.mixemulator.vm.io.file

import java.io.{BufferedWriter, File, FileWriter}

import org.linnando.mixemulator.vm.exceptions.EndOfFileException
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.CardReader
import org.specs2.matcher.ContentMatchers
import org.specs2.mutable.Specification

import scala.collection.immutable.Queue

class FileCardReaderSpec extends Specification with ContentMatchers {
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
      val device = FileCardReader(filename)
      device.blockSize must be equalTo CardReader.BLOCK_SIZE
      device.filename must be equalTo filename
      device.tasks.isCompleted must beTrue
      device.isBusy must beFalse
      device.pos must be equalTo 0L
    }

    "input data from a file" in {
      val filename = "cardreader1"
      val file = new File(filename)
      val writer = new BufferedWriter(new FileWriter(file))
      writer.write(line0)
      writer.newLine()
      writer.write(line1)
      writer.newLine()
      writer.close()
      val device = FileCardReader(filename)
      val busyState = device.read().read()
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 2L
      val finalState = busyState.flush()
      finalState._1.isBusy must beFalse
      finalState._1.pos must be equalTo 2L
      finalState._2 must be equalTo Queue(words0, words1)
      file must haveSameLinesAs(Seq(line0, line1))
      file.delete()
    }

    "throw an exception when the file end is reached" in {
      val filename = "cardreader2"
      val file = new File(filename)
      val writer = new BufferedWriter(new FileWriter(file))
      writer.close()
      val device = FileCardReader(filename)
      val busyState = device.read()
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      busyState.flush() must throwAn[EndOfFileException]
      file.delete()
    }
  }
}
