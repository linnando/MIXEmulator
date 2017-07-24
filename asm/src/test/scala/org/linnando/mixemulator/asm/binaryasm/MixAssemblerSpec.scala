package org.linnando.mixemulator.asm.binaryasm

import org.linnando.mixemulator.asm.MixAssembler
import org.linnando.mixemulator.asm.exceptions._
import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.binary.BinaryVirtualMachineBuilder
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.mutable.Specification

class MixAssemblerSpec extends Specification {
  private val builder = binary.createVirtualMachineBuilder()
  private val assembler = MixAssembler(builder, List(List(0)))

  "binary MIX assembler" should {
    "skip a comment line" in {
      val nextState = assembler.parseLine(" * COMMENT")
      nextState.builder.build.currentState must be equalTo builder.build.currentState
      nextState.counterHistory must be equalTo List(List(0, 0))
    }

    "throw an exception on a wrong line" in {
      assembler.parseLine("abc") must throwA[WrongLineException]
    }

    "assign a symbol" in {
      val nextState = assembler.parseLine("ABC EQU 2000")
      nextState.builder match {
        case b: BinaryVirtualMachineBuilder =>
          b.symbols must havePair("ABC", binary.MixWord(2000))
      }
      nextState.counterHistory must be equalTo List(List(0, 0))
    }

    "throw an exception without a symbol value" in {
      assembler.parseLine("ABC EQU   ") must throwA[WrongAddressPartException]
    }

    "change the current address counter value" in {
      val nextState = assembler.parseLine(" ORIG 2000")
      nextState.builder.getCounter must be equalTo 2000
      nextState.counterHistory must be equalTo List(List(2000), List(0))
    }

    "throw an exception without an address counter value" in {
      assembler.parseLine(" ORIG ") must throwA[WrongAddressPartException]
      assembler.parseLine("ABC ORIG ") must throwA[WrongAddressPartException]
    }

    "store a constant" in {
      val nextState = assembler.parseLine(" CON 2000")
      nextState.builder match {
        case b: BinaryVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 31, 16))
      }
      nextState.counterHistory must be equalTo List(List(1, 0))
    }

    "throw an exception without a value of a constant" in {
      assembler.parseLine(" CON ") must throwA[WrongAddressPartException]
      assembler.parseLine("ABC CON ") must throwA[WrongAddressPartException]
    }

    "store a character constant separated with one space" in {
      val nextState = assembler.parseLine(" ALF ABCDE")
      nextState.builder match {
        case b: BinaryVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(1, 2, 3, 4, 5))
      }
      nextState.counterHistory must be equalTo List(List(1, 0))
    }

    "store a character constant separated with two spaces" in {
      val nextState = assembler.parseLine(" ALF   ABCD")
      nextState.builder match {
        case b: BinaryVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 1, 2, 3, 4))
      }
      nextState.counterHistory must be equalTo List(List(1, 0))
    }

    "throw an exception without a value of a character constant" in {
      assembler.parseLine(" ALF ") must throwA[WrongAddressPartException]
      assembler.parseLine("ABC ALF ") must throwA[WrongAddressPartException]
    }

    "translate a command" in {
      val nextState = assembler.parseLine(" LDA 2000")
      nextState.builder match {
        case b: BinaryVirtualMachineBuilder =>
          b.state.get(0) must be equalTo IOWord(negative = false, Seq(31, 16, 0, 5, 8))
      }
      nextState.counterHistory must be equalTo List(List(1, 0))
    }

    "throw an exception on a wrong operator" in {
      assembler.parseLine(" LDZ 1000") must throwA[WrongOperatorException]
    }

    "throw an exception with a wrong address part format" in {
      assembler.parseLine(" LDA 0,") must throwA[WrongAddressPartException]
      assembler.parseLine(" LDA 0()") must throwA[WrongAddressPartException]
    }

    "throw an exception on an attempt to set a field specification when it is used to distinguish commands" in {
      assembler.parseLine(" HLT 0,0(0)") must throwA[FixedFieldSpecException]
    }

    "generate a final section with undefined symbols and literals" in {
      val nextState = assembler
        .parseLine(" ORIG 3000")
        .parseLine(" LDA =-2000=,1(1:3)")
        .parseLine(" STA ABC")
        .parseLine(" END 3000")
      nextState.builder match {
        case b: BinaryVirtualMachineBuilder =>
          b.state.get(3000) must be equalTo IOWord(negative = false, Seq(46, 59, 1, 11, 8))
          b.state.get(3001) must be equalTo IOWord(negative = false, Seq(46, 58, 0, 5, 24))
          b.state.get(3002) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
          b.state.get(3003) must be equalTo IOWord(negative = true, Seq(0, 0, 0, 31, 16))
          b.state.programCounter must be equalTo binary.MixIndex(3000)
      }
      nextState.counterHistory must be equalTo List(List(3004, 3002, 3001, 3000), List(0))
    }

    "throw an exception without a program start address" in {
      assembler.parseLine(" END ") must throwA[WrongAddressPartException]
    }

    "translate a whole program" in {
      val program = MixAssembler.translate(
        binary.createVirtualMachineBuilder(),
        Seq(
          " ORIG 3000",
          " LDA =-2000=,1(1:3)",
          " STA ABC",
          " END 3000"
        )
      )
      program._1.currentState.get(3000) must be equalTo IOWord(negative = false, Seq(46, 59, 1, 11, 8))
      program._1.currentState.get(3001) must be equalTo IOWord(negative = false, Seq(46, 58, 0, 5, 24))
      program._1.currentState.get(3002) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
      program._1.currentState.get(3003) must be equalTo IOWord(negative = true, Seq(0, 0, 0, 31, 16))
      program._1.currentState.getProgramCounter must be equalTo 3000
      program._2 must be equalTo List(List(3004, 3002, 3001, 3000), List(0))
    }
  }
}
