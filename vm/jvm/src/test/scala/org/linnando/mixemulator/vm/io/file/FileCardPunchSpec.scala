package org.linnando.mixemulator.vm.io.file

import java.io.File

import org.linnando.mixemulator.vm.io.CardPunch
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{ContentMatchers, FileMatchers}
import org.specs2.mutable.Specification

class FileCardPunchSpec(implicit ee: ExecutionEnv) extends Specification with FileMatchers with ContentMatchers {
  private val lowLevelOps: LineAccessFileOutputOps = LineAccessFileOutputOps.create()

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
      val device = FileCardPunch.create(filename, lowLevelOps)
      device.blockSize must be equalTo CardPunch.BLOCK_SIZE
      device.filename must be equalTo filename
      device.version must be equalTo 0
      device.isBusy must beFalse
      new File(s"$filename/0").delete()
      new File(filename).delete()
    }

    "output data to a file" in {
      val filename = "cardpunch1"
      val device = FileCardPunch.create(filename, lowLevelOps)
      val busyState = device.write(words0).write(words1)
      busyState.version must be equalTo 2
      busyState.isBusy must beTrue
      val finalState = busyState.flush()
      finalState.map(_._1.version) must beEqualTo(2).await
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._2) must beNone.await
      val file1 = new File(s"$filename/1")
      file1 must exist
      file1 must haveSameLinesAs(Seq(line0))
      val file2 = new File(s"$filename/2")
      file2 must exist
      file2 must haveSameLinesAs(Seq(line0, line1))
      new File(s"$filename/0").delete()
      file1.delete()
      file2.delete()
      new File(filename).delete()
    }
  }
}
