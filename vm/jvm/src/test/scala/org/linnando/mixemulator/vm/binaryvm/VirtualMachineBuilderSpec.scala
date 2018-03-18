package org.linnando.mixemulator.vm.binaryvm

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.binary.BinaryVirtualMachineBuilder
import org.linnando.mixemulator.vm.exceptions._
import org.linnando.mixemulator.vm.io.data.IOWord
import org.specs2.mutable.Specification

class VirtualMachineBuilderSpec extends Specification {
  "binary virtual machine builder" should {
    "assign a value to a global symbol" in {
      binary.createVirtualMachineBuilder()
        .withWValueSymbol("ABC", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.symbols ("ABC") must be equalTo binary.MixWord (0x41403144)
          builder.getCounter must be equalTo 0
      }
    }

    "not allow redefining a global symbol" in {
      val builder = binary.createVirtualMachineBuilder()
        .withWValueSymbol("ABC", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      builder.withWValueSymbol("ABC", "1000") must throwA[DuplicateSymbolException].like {
        case e: DuplicateSymbolException => e.symbol must be equalTo "ABC"
      }
    }

    "assign a value to a local symbol" in {
      binary.createVirtualMachineBuilder()
        .withWValueSymbol("0H", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.symbols ("0H") must be equalTo binary.MixWord (0x41403144)
          builder.getCounter must be equalTo 0
      }
    }

    "allow redefining a local symbol" in {
      binary.createVirtualMachineBuilder()
        .withWValueSymbol("0H", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withWValueSymbol("0H", "-2000(0:2),3000(4:5)") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.symbols("0H") must be equalTo binary.MixWord(0x5f400bb8)
          builder.getCounter must be equalTo 0
      }
    }

    "not allow using local references as defined symbols" in {
      val builder = binary.createVirtualMachineBuilder()
      builder.withWValueSymbol("0F", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)") must throwA[WrongLabelException].like {
        case e: WrongLabelException => e.label must be equalTo "0F"
      }
      builder.withWValueSymbol("0B", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)") must throwA[WrongLabelException].like {
        case e: WrongLabelException => e.label must be equalTo "0B"
      }
    }

    "evaluate correct expressions" in {
      binary.createVirtualMachineBuilder()
        .withOrig("2000")
        .withWValueSymbol("A", "-1+5")
        .withWValueSymbol("B", "-1+5*20/6")
        .withWValueSymbol("C", "1//3")
        .withWValueSymbol("D", "1:3")
        .withWValueSymbol("E", "*-3")
        .withWValueSymbol("F", "***") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.symbols("A") must be equalTo binary.MixWord(4)
          builder.symbols("B") must be equalTo binary.MixWord(13)
          builder.symbols("C") must be equalTo binary.MixWord(0x15555555)
          builder.symbols("D") must be equalTo binary.MixWord(11)
          builder.symbols("E") must be equalTo binary.MixWord(1997)
          builder.symbols("F") must be equalTo binary.MixWord(4000000)
      }
    }

    "throw an exception for invalid expressions" in {
      val builder = binary.createVirtualMachineBuilder()
      builder.withWValueSymbol("ABC", "/2000") must throwAn[InvalidExpressionException].like {
        case e: InvalidExpressionException => e.expression must be equalTo "/2000"
      }
      builder.withWValueSymbol("ABC", "2000\\a") must throwAn[InvalidExpressionException].like {
        case e: InvalidExpressionException => e.expression must be equalTo "2000\\a"
      }
      builder.withWValueSymbol("ABC", "3+2H") must throwAn[InvalidExpressionException].like {
        case e: InvalidExpressionException => e.expression must be equalTo "2H"
      }
      builder.withWValueSymbol("ABC", "2000,1(3:5") must throwAn[InvalidExpressionException].like {
        case e: InvalidExpressionException => e.expression must be equalTo "1(3:5"
      }
    }

    "throw an exception when an undefined symbol is used in an expression" in {
      val builder = binary.createVirtualMachineBuilder()
      builder.withWValueSymbol("ABC", "2B-1") must throwAn[UndefinedSymbolException].like {
        case e: UndefinedSymbolException => e.symbol must be equalTo "2H"
      }
      builder.withWValueSymbol("ABC", "2*2F") must throwAn[UndefinedSymbolException].like {
        case e: UndefinedSymbolException => e.symbol must be equalTo "2H"
      }
      builder.withWValueSymbol("ABC", "DEF+5") must throwAn[UndefinedSymbolException].like {
        case e: UndefinedSymbolException => e.symbol must be equalTo "DEF"
      }
    }

    "assign the address current counter value to a symbol" in {
      binary.createVirtualMachineBuilder()
        .withOrig("2000")
        .withCurrentCounterSymbol("ABC") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.symbols("ABC") must be equalTo binary.MixWord(2000)
      }
    }

    "change the current address counter value" in {
      binary.createVirtualMachineBuilder()
        .withOrig("2000") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.getCounter must be equalTo 2000
      }
    }

    "throw an exception if the memory address is too big" in {
      val builder = binary.createVirtualMachineBuilder()
      builder.withOrig("4000") must throwA[WrongMemoryAddressException].like {
        case e: WrongMemoryAddressException => e.address must be equalTo 4000
      }
    }

    "store a value in memory" in {
      binary.createVirtualMachineBuilder()
        .withConstant("-2000") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(0) must be equalTo IOWord(negative = true, Seq(0, 0, 0, 31, 16))
          builder.getCounter must be equalTo 1
      }
    }

    "store a character constant in memory" in {
      binary.createVirtualMachineBuilder()
        .withCharCode(" ABCD") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 1, 2, 3, 4))
          builder.getCounter must be equalTo 1
      }
    }

    "throw an exception when unsupported characters are encountered" in {
      val builder = binary.createVirtualMachineBuilder()
      builder.withCharCode(" AbCd") must throwAn[UnsupportedCharacterException].like {
        case e: UnsupportedCharacterException => e.char must be equalTo 'b'
      }
    }

    "not store a value outside the memory" in {
      val builder = binary.createVirtualMachineBuilder()
        .withOrig("3999")
        .withConstant("-2000")
      builder.withCharCode(" ABCD") must throwA[WrongMemoryAddressException].like {
        case e: WrongMemoryAddressException => e.address must be equalTo 4000
      }
    }

    "store a command in memory" in {
      binary.createVirtualMachineBuilder()
        .withCommand("2000", null, null, 8, 5) match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(0) must be equalTo IOWord(negative = false, Seq(31, 16, 0, 5, 8))
          builder.getCounter must be equalTo 1
      }
    }

    "evaluate an expression in the address part of a command in memory" in {
      binary.createVirtualMachineBuilder()
        .withCommand("2000+2", null, null, 8, 5) match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(0) must be equalTo IOWord(negative = false, Seq(31, 18, 0, 5, 8))
          builder.getCounter must be equalTo 1
      }
    }

    "store forward references" in {
      binary.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5) match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 5, 8))
          builder.forwardReferences must haveKey("ABC")
          builder.forwardReferences("ABC") must be equalTo Seq(binary.MixIndex(0))
      }
    }

    "substitute forward references after the global symbol is defined" in {
      binary.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
        .withWValueSymbol("ABC", "2000") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(0) must be equalTo IOWord(negative = false, Seq(31, 16, 0, 5, 8))
          builder.forwardReferences must not haveKey "ABC"
      }
    }

    "throw an exception if the symbol defined for a forward reference is too big" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
      builder.withWValueSymbol("ABC", "2000(1:2)") must throwAn[OverflowException]
    }

    "store literals" in {
      binary.createVirtualMachineBuilder()
        .withCommand("=2000=", null, null, 8, 5) match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(0) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 5, 8))
          builder.getCounter must be equalTo 1
          builder.literals must haveKey(binary.MixWord(2000))
          builder.literals(binary.MixWord(2000)) must be equalTo Seq(binary.MixIndex(0))
      }
    }

    "generate a final section with undefined symbols and literals" in {
      binary.createVirtualMachineBuilder()
        .withOrig("3000")
        .withCommand("=-2000=", "1", "1:3", 8, 5)
        .withCommand("ABC", null, null, 24, 5)
        .withFinalSection(null, "3000-1") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(3000) must be equalTo IOWord(negative = false, Seq(46, 59, 1, 11, 8))
          builder.state.get(3001) must be equalTo IOWord(negative = false, Seq(46, 58, 0, 5, 24))
          builder.state.get(3002) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
          builder.state.get(3003) must be equalTo IOWord(negative = true, Seq(0, 0, 0, 31, 16))
          builder.getCounter must be equalTo 3004
          builder.state.programCounter must be equalTo binary.MixIndex(2999)
      }
    }

    "treat final section label as a usual symbol" in {
      binary.createVirtualMachineBuilder()
        .withOrig("3000")
        .withCommand("=-2000=", "1", "1:3", 8, 5)
        .withCommand("ABC", null, null, 24, 5)
        .withFinalSection("ABC", "0") match {
        case builder: BinaryVirtualMachineBuilder =>
          builder.state.get(3000) must be equalTo IOWord(negative = false, Seq(46, 58, 1, 11, 8))
          builder.state.get(3001) must be equalTo IOWord(negative = false, Seq(46, 59, 0, 5, 24))
          builder.state.get(3002) must be equalTo IOWord(negative = true, Seq(0, 0, 0, 31, 16))
          builder.state.get(3003) must be equalTo IOWord(negative = false, Seq(0, 0, 0, 0, 0))
          builder.getCounter must be equalTo 3003
          builder.state.programCounter must be equalTo binary.MixIndex(0)
      }
    }

    "build a non-tracking virtual machine" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", null, null, 8, 5)
        .withFinalSection(null, "3000")
        .build
      machine.currentState.get(2000) must be equalTo IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.get(3000) must be equalTo IOWord(negative = false, Seq(31, 16, 0, 5, 8))
      machine.currentState.getProgramCounter must be equalTo 3000
    }

    "build a tracking virtual machine" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", null, null, 8, 5)
        .withFinalSection(null, "3000")
        .buildTracking
      machine.currentState.get(2000) must be equalTo IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.get(3000) must be equalTo IOWord(negative = false, Seq(31, 16, 0, 5, 8))
      machine.currentState.getProgramCounter must be equalTo 3000
    }
  }
}
