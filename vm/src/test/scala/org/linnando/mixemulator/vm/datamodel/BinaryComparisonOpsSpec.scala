package org.linnando.mixemulator.vm.datamodel

import org.linnando.mixemulator.vm.BinaryVirtualMachine.{BinaryMixIndex, BinaryMixWord}
import org.linnando.mixemulator.vm.Comparison
import org.specs2.mutable.Specification

class BinaryComparisonOpsSpec extends Specification {

  "binary word comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      BinaryMixWord(0x2) <=> BinaryMixWord(0x1) must be equalTo Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      BinaryMixWord(0x1) <=> BinaryMixWord(0x2) must be equalTo Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      BinaryMixWord(0x1) <=> BinaryMixWord(0x1) must be equalTo Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      BinaryMixWord(0x40000001) <=> BinaryMixWord(0x40000001) must be equalTo Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      BinaryMixWord(0x1) <=> BinaryMixWord(0x40000002) must be equalTo Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      BinaryMixWord(0x40000001) <=> BinaryMixWord(0x2) must be equalTo Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      BinaryMixWord(0x20000000) <=> BinaryMixWord(0x60000000) must be equalTo Comparison.GREATER
    }
  }

  "binary index comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      BinaryMixIndex(0x2) <=> BinaryMixWord(0x1) must be equalTo Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      BinaryMixIndex(0x1) <=> BinaryMixWord(0x2) must be equalTo Comparison.LESS
    }

    "compare a bigger negative number to a smaller negative number" in {
      BinaryMixIndex(0x1001) <=> BinaryMixWord(0x40000002) must be equalTo Comparison.GREATER
    }

    "compare a smaller negative number to a bigger negative number" in {
      BinaryMixIndex(0x1002) <=> BinaryMixWord(0x40000001) must be equalTo Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      BinaryMixIndex(0x1) <=> BinaryMixWord(0x1) must be equalTo Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      BinaryMixIndex(0x1001) <=> BinaryMixWord(0x40000001) must be equalTo Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      BinaryMixIndex(0x1) <=> BinaryMixWord(0x40000002) must be equalTo Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      BinaryMixIndex(0x1001) <=> BinaryMixWord(0x2) must be equalTo Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      BinaryMixIndex(0x800) <=> BinaryMixWord(0x7ffff800) must be equalTo Comparison.GREATER
    }
  }

}
