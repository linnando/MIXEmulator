package org.linnando.mixemulator.vm.binaryvm

import org.linnando.mixemulator.vm.binary
import org.linnando.mixemulator.vm.binary.BinaryVirtualMachineBuilder
import org.linnando.mixemulator.vm.exceptions._
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalatest.Inside
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VirtualMachineBuilderSpec extends AnyWordSpec with Matchers with Inside {
  "binary virtual machine builder" should {
    "assign a value to a global symbol" in {
      val builder = binary.createVirtualMachineBuilder()
        .withWValueSymbol("ABC", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.symbols("ABC") mustEqual binary.MixWord(0x41403144)
        b.getCounter mustEqual 0
      }
    }

    "not allow redefining a global symbol" in {
      val builder = binary.createVirtualMachineBuilder()
        .withWValueSymbol("ABC", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      val thrown = the[DuplicateSymbolException] thrownBy builder.withWValueSymbol("ABC", "1000")
      thrown.symbol mustEqual "ABC"
    }

    "assign a value to a local symbol" in {
      val builder = binary.createVirtualMachineBuilder()
        .withWValueSymbol("0H", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.symbols("0H") mustEqual binary.MixWord(0x41403144)
        b.getCounter mustEqual 0
      }
    }

    "allow redefining a local symbol" in {
      val builder = binary.createVirtualMachineBuilder()
        .withWValueSymbol("0H", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withWValueSymbol("0H", "-2000(0:2),3000(4:5)")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.symbols("0H") mustEqual binary.MixWord(0x5f400bb8)
        b.getCounter mustEqual 0
      }
    }

    "not allow using local forward references as defined symbols" in {
      val thrown = the[WrongLabelException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("0F", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      }
      thrown.label mustEqual "0F"
    }

    "not allow using local backward references as defined symbols" in {
      val thrown = the[WrongLabelException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("0B", "-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
      }
      thrown.label mustEqual "0B"
    }

    "evaluate correct expressions" in {
      val builder = binary.createVirtualMachineBuilder()
        .withOrig("2000")
        .withWValueSymbol("A", "-1+5")
        .withWValueSymbol("B", "-1+5*20/6")
        .withWValueSymbol("C", "1//3")
        .withWValueSymbol("D", "1:3")
        .withWValueSymbol("E", "*-3")
        .withWValueSymbol("F", "***")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.symbols("A") mustEqual binary.MixWord(4)
        b.symbols("B") mustEqual binary.MixWord(13)
        b.symbols("C") mustEqual binary.MixWord(0x15555555)
        b.symbols("D") mustEqual binary.MixWord(11)
        b.symbols("E") mustEqual binary.MixWord(1997)
        b.symbols("F") mustEqual binary.MixWord(4000000)
      }
    }

    "throw an exception for a lacking operand" in {
      val thrown = the[InvalidExpressionException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("ABC", "/2000")
      }
      thrown.expression mustEqual "/2000"
    }

    "throw an exception for an invalid operator" in {
      val thrown = the[InvalidExpressionException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("ABC", "2000\\a")
      }
      thrown.expression mustEqual "2000\\a"
    }

    "throw an exception for a local reference definition in an expression" in {
      val thrown = the[InvalidExpressionException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("ABC", "3+2H")
      }
      thrown.expression mustEqual "2H"
    }

    "throw an exception for an invalid field specification" in {
      val thrown = the[InvalidExpressionException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("ABC", "2000,1(3:5")
      }
      thrown.expression mustEqual "1(3:5"
    }

    "throw an exception when an undefined local backward reference is used in an expression" in {
      val thrown = the[UndefinedSymbolException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("ABC", "2B-1")
      }
      thrown.symbol mustEqual "2H"
    }

    "throw an exception when an undefined local forward reference is used in an expression" in {
      val thrown = the[UndefinedSymbolException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("ABC", "2*2F")
      }
      thrown.symbol mustEqual "2H"
    }

    "throw an exception when an undefined global symbol is used in an expression" in {
      val thrown = the[UndefinedSymbolException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withWValueSymbol("ABC", "DEF+5")
      }
      thrown.symbol mustEqual "DEF"
    }

    "assign the address current counter value to a symbol" in {
      val builder = binary.createVirtualMachineBuilder()
        .withOrig("2000")
        .withCurrentCounterSymbol("ABC")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.symbols("ABC") mustEqual binary.MixWord(2000)
      }
    }

    "change the current address counter value" in {
      val builder = binary.createVirtualMachineBuilder()
        .withOrig("2000")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.getCounter mustEqual 2000
      }
    }

    "throw an exception if the memory address is too big" in {
      val thrown = the[WrongMemoryAddressException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withOrig("4000")
      }
      thrown.address mustEqual 4000
    }

    "store a value in memory" in {
      val builder = binary.createVirtualMachineBuilder()
        .withConstant("-2000")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = true, Seq(0, 0, 0, 31, 16))
        b.getCounter mustEqual 1
      }
    }

    "store a character constant in memory" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCharCode(" ABCD")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 1, 2, 3, 4))
        b.getCounter mustEqual 1
      }
    }

    "throw an exception when unsupported characters are encountered" in {
      val thrown = the[UnsupportedCharacterException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withCharCode(" AbCd")
      }
      thrown.char mustEqual 'b'
    }

    "not store a value outside the memory" in {
      val thrown = the[WrongMemoryAddressException] thrownBy {
        binary.createVirtualMachineBuilder()
          .withOrig("3999")
          .withConstant("-2000")
          .withCharCode(" ABCD")
      }
      thrown.address mustEqual 4000
    }

    "store a command in memory" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCommand("2000", null, null, 8, 5)
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(31, 16, 0, 5, 8))
        b.getCounter mustEqual 1
      }
    }

    "evaluate an expression in the address part of a command in memory" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCommand("2000+2", null, null, 8, 5)
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(31, 18, 0, 5, 8))
        b.getCounter mustEqual 1
      }
    }

    "store forward references" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 0, 0, 5, 8))
        b.forwardReferences must contain("ABC", Seq(binary.MixIndex(0)))
      }
    }

    "substitute forward references after the global symbol is defined" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
        .withWValueSymbol("ABC", "2000")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(31, 16, 0, 5, 8))
        b.forwardReferences must not contain key("ABC")
      }
    }

    "throw an exception if the symbol defined for a forward reference is too big" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCommand("ABC", null, null, 8, 5)
      an[OverflowException] must be thrownBy builder.withWValueSymbol("ABC", "2000(1:2)")
    }

    "store literals" in {
      val builder = binary.createVirtualMachineBuilder()
        .withCommand("=2000=", null, null, 8, 5)
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(0) mustEqual IOWord(negative = false, Seq(0, 0, 0, 5, 8))
        b.getCounter mustEqual 1
        b.literals must contain(binary.MixWord(2000), Seq(binary.MixIndex(0)))
      }
    }

    "generate a final section with undefined symbols and literals" in {
      val builder = binary.createVirtualMachineBuilder()
        .withOrig("3000")
        .withCommand("=-2000=", "1", "1:3", 8, 5)
        .withCommand("ABC", null, null, 24, 5)
        .withFinalSection(null, "3000-1")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(3000) mustEqual IOWord(negative = false, Seq(46, 59, 1, 11, 8))
        b.state.get(3001) mustEqual IOWord(negative = false, Seq(46, 58, 0, 5, 24))
        b.state.get(3002) mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
        b.state.get(3003) mustEqual IOWord(negative = true, Seq(0, 0, 0, 31, 16))
        b.getCounter mustEqual 3004
        b.state.programCounter mustEqual binary.MixIndex(2999)
      }
    }

    "treat final section label as a usual symbol" in {
      val builder = binary.createVirtualMachineBuilder()
        .withOrig("3000")
        .withCommand("=-2000=", "1", "1:3", 8, 5)
        .withCommand("ABC", null, null, 24, 5)
        .withFinalSection("ABC", "0")
      inside(builder) { case b: BinaryVirtualMachineBuilder =>
        b.state.get(3000) mustEqual IOWord(negative = false, Seq(46, 58, 1, 11, 8))
        b.state.get(3001) mustEqual IOWord(negative = false, Seq(46, 59, 0, 5, 24))
        b.state.get(3002) mustEqual IOWord(negative = true, Seq(0, 0, 0, 31, 16))
        b.state.get(3003) mustEqual IOWord(negative = false, Seq(0, 0, 0, 0, 0))
        b.getCounter mustEqual 3003
        b.state.programCounter mustEqual binary.MixIndex(0)
      }
    }

    "build a non-tracking virtual machine" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", null, null, 8, 5)
        .withFinalSection(null, "3000")
        .build
      machine.currentState.get(2000) mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.get(3000) mustEqual IOWord(negative = false, Seq(31, 16, 0, 5, 8))
      machine.currentState.getProgramCounter mustEqual 3000
    }

    "build a tracking virtual machine" in {
      val machine = binary.createVirtualMachineBuilder()
        .withOrig("2000").withConstant("-1(0:1),16(2:2),3(3:3),5(4:4),4(5:5)")
        .withOrig("3000").withCommand("2000", null, null, 8, 5)
        .withFinalSection(null, "3000")
        .buildTracking
      machine.currentState.get(2000) mustEqual IOWord(negative = true, Seq(1, 16, 3, 5, 4))
      machine.currentState.get(3000) mustEqual IOWord(negative = false, Seq(31, 16, 0, 5, 8))
      machine.currentState.getProgramCounter mustEqual 3000
    }
  }
}
