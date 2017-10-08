package org.linnando.mixemulator.asm.decimalasm

import org.linnando.mixemulator.asm.MixDisassembler
import org.linnando.mixemulator.vm.decimal
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.mutable.Specification

class MixDisassemblerSpec extends Specification {
  private val disassembler = new MixDisassembler(decimal)

  "decimal MIX disassembler" should {
    "disassemble a command" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(20, 0, 1, 13, 8)))
      line must be equalTo "           LDA  2000,1(1:5)"
    }

    "preserve a zero address" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(0, 0, 1, 13, 8)))
      line must be equalTo "           LDA  0,1(1:5)"
    }

    "omit a zero index specification" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(20, 0, 0, 13, 8)))
      line must be equalTo "           LDA  2000(1:5)"
    }

    "omit a default field specification" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(20, 0, 1, 5, 8)))
      line must be equalTo "           LDA  2000,1"
    }

    "disassemble a command with a negative address" in {
      val line = disassembler.disassembleLine(IOWord(negative = true, Seq(20, 0, 0, 5, 8)))
      line must be equalTo "           LDA  -2000"
    }

    "disassemble a command when field specification is used to distinguish operators" in {
      val line = disassembler.disassembleLine(IOWord(negative = false, Seq(20, 0, 0, 1, 48)))
      line must be equalTo "           DECA 2000"
    }
  }
}
