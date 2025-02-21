package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.Comparison
import org.linnando.mixemulator.vm.decimal._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ComparisonOpsSpec extends AsyncWordSpec with Matchers {
  "decimal word comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      MixWord(2) <=> MixWord(1) mustEqual Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      MixWord(1) <=> MixWord(2) mustEqual Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      MixWord(1) <=> MixWord(1) mustEqual Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      MixWord(0x400000001L) <=> MixWord(0x400000001L) mustEqual Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      MixWord(1) <=> MixWord(0x400000002L) mustEqual Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      MixWord(0x400000001L) <=> MixWord(2) mustEqual Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      MixWord(5000000000L) <=> MixWord(0x400000000L + 5000000000L) mustEqual Comparison.GREATER
    }
  }

  "decimal index comparison" should {
    "compare a bigger positive number to a smaller positive number" in {
      MixIndex(2) <=> MixWord(1) mustEqual Comparison.GREATER
    }

    "compare a smaller positive number to a bigger positive number" in {
      MixIndex(1) <=> MixWord(2) mustEqual Comparison.LESS
    }

    "compare a bigger negative number to a smaller negative number" in {
      MixIndex(0x4001) <=> MixWord(0x400000002L) mustEqual Comparison.GREATER
    }

    "compare a smaller negative number to a bigger negative number" in {
      MixIndex(0x4002) <=> MixWord(0x400000001L) mustEqual Comparison.LESS
    }

    "define that a positive number equals to itself" in {
      MixIndex(1) <=> MixWord(1) mustEqual Comparison.EQUAL
    }

    "define that a negative number equals to itself" in {
      MixIndex(0x4001) <=> MixWord(0x400000001L) mustEqual Comparison.EQUAL
    }

    "define that a positive number is greater than a negative number" in {
      MixIndex(1) <=> MixWord(0x400000002L) mustEqual Comparison.GREATER
    }

    "define that a negative number is less than a positive number" in {
      MixIndex(0x4001) <=> MixWord(2) mustEqual Comparison.LESS
    }

    "compare a positive and a negative numbers when subtraction would give an overflow" in {
      MixIndex(5000) <=> MixWord(0x400000000L + 9999995000L) mustEqual Comparison.GREATER
    }
  }
}
