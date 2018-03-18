package org.linnando.mixemulator.vm.io.file

import java.io.File

import org.linnando.mixemulator.vm.exceptions.EndOfFileException
import org.linnando.mixemulator.vm.io.PaperTape
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.ContentMatchers
import org.specs2.mutable.Specification

class FilePaperTapeSpec(implicit ee: ExecutionEnv) extends Specification with ContentMatchers {
  val line0 = "0123456789012345678901234567890123456789012345678901234567890123456789"
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
    IOWord(Seq('5', '6', '7', '8', '9'))
  )

  val line1 = "5678901234567890123456789012345678901234567890123456789012345678901234"
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
    IOWord(Seq('0', '1', '2', '3', '4'))
  )

  "file emulator of a paper tape" should {
    "create a device with correct parameters" in {
      val filename = "papertape0"
      val device = FilePaperTape.create(filename, "")
      device.blockSize must be equalTo PaperTape.BLOCK_SIZE
      device.filename must be equalTo filename
      device.isBusy must beFalse
      device.pos must be equalTo 0L
      new File(filename).delete()
    }

    "input data from a file" in {
      val filename = "papertape1"
      val file = new File(filename)
      val device = FilePaperTape.create(filename, s"$line0\n$line1\n")
      val busyState = device.read()
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      val finalState = busyState.flush()
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(1L).await
      finalState.map(_._2) must beSome(words0).await
      file must haveSameLinesAs(Seq(line0, line1))
      file.delete()
    }

    "input multiple lines from a file" in {
      val filename = "papertape2"
      val file = new File(filename)
      val device = FilePaperTape.create(filename, s"$line0\n$line1\n")
      val busyState = device.read().flush().map(_._1.read())
      busyState.map(_.isBusy) must beTrue.await
      busyState.map(_.pos) must beEqualTo(2L).await
      val finalState = busyState.flatMap(_.flush())
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(2L).await
      finalState.map(_._2) must beSome(words1).await
      file must haveSameLinesAs(Seq(line0, line1))
      file.delete()
    }

    "throw an exception when the file end is reached" in {
      val filename = "papertape3"
      val file = new File(filename)
      val device = FilePaperTape.create(filename, "")
      val busyState = device.read()
      busyState.isBusy must beTrue
      busyState.pos must be equalTo 1L
      busyState.flush() must throwAn[EndOfFileException].await
      file.delete()
    }

    "reset the reading position" in {
      val filename = "papertape4"
      val file = new File(filename)
      val device = FilePaperTape.create(filename, s"$line0\n")
      val busyState = device.read().flush().map(_._1 match {
        case d: FilePaperTape => d.reset().read()
      })
      busyState.map(_.isBusy) must beTrue.await
      busyState.map(_.pos) must beEqualTo(1L).await
      val finalState = busyState.flatMap(_.flush())
      finalState.map(_._1.isBusy) must beFalse.await
      finalState.map(_._1.pos) must beEqualTo(1L).await
      finalState.map(_._2) must beSome(words0).await
      file must haveSameLinesAs(Seq(line0))
      file.delete()
    }
  }
}
