package org.linnando.mixemulator.vm.io.file

import java.io.{BufferedWriter, File, FileWriter}

import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.CardPunch
import org.specs2.matcher.{ContentMatchers, FileMatchers}
import org.specs2.mutable.Specification

class FileCardPunchSpec extends Specification with FileMatchers with ContentMatchers {
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

  "file emulator of a card punch" should {
    "create a device with correct parameters" in {
      val filename = "cardpunch0"
      val device = FileCardPunch(filename)
      device.blockSize must be equalTo CardPunch.BLOCK_SIZE
      device.filename must be equalTo filename
      device.version must be equalTo 0
      device.tasks.isCompleted must beTrue
      device.isBusy must beFalse
    }

    "output data to a file" in {
      val filename = "cardpunch1"
      val device = FileCardPunch(filename)
      val busyState = device.write(words0).write(words1)
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState._1.version must be equalTo 2
      finalState._1.isBusy must beFalse
      finalState._2 must beEmpty
      val file1 = new File(s"$filename.1")
      file1 must exist
      file1 must haveSameLinesAs(Seq(line0))
      val file2 = new File(s"$filename.2")
      file2 must exist
      file2 must haveSameLinesAs(Seq(line0, line1))
      file1.delete()
      file2.delete()
    }

    "append data to an existing file" in {
      val filename = "test"
      val file0 = new File(s"$filename.0")
      val writer = new BufferedWriter(new FileWriter(file0))
      writer.write(line0)
      writer.newLine()
      writer.close()
      val device = FileCardPunch(filename)
      val busyState = device.write(words1)
      busyState.version must be equalTo 1
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState._1.version must be equalTo 1
      finalState._1.isBusy must beFalse
      finalState._2 must beEmpty
      val file1 = new File(s"$filename.1")
      file1 must exist
      file1 must haveSameLinesAs(Seq(line0, line1))
      file0.delete()
      file1.delete()
    }
  }
}
