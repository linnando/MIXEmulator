package org.linnando.mixemulator.vm.decimalvm.datamodel

import org.linnando.mixemulator.vm.decimal._
import org.linnando.mixemulator.vm.exceptions.{DivisionByZeroException, OverflowException}
import org.specs2.mutable.Specification

class ArithmeticOpsSpec extends Specification {

  "decimal word negation" should {
    "negate a positive number to a negative number" in {
      -MixWord(1) must be equalTo MixWord(0x400000001L)
    }

    "negate a negative number to a positive number" in {
      -MixWord(0x400000001L) must be equalTo MixWord(1)
    }

    "negate the positive zero to the negative zero" in {
      -MixWord(0) must be equalTo MixWord(0x400000000L)
    }

    "negate the negative zero to the positive zero" in {
      -MixWord(0x400000000L) must be equalTo MixWord(0)
    }
  }

  "decimal word addition" should {
    "sum two positive numbers" in {
      MixWord(1) + MixWord(2) must be equalTo (false, MixWord(3))
    }

    "sum two negative numbers" in {
      MixWord(0x400000001L) + MixWord(0x400000002L) must be equalTo (false, MixWord(0x400000003L))
    }

    "sum two positive numbers with an overflow" in {
      MixWord(5000000000L) + MixWord(5000000000L) must be equalTo (true, MixWord(0))
    }

    "sum a positive number and a negative number smaller in absolute value" in {
      MixWord(2) + MixWord(0x400000001L) must be equalTo (false, MixWord(1))
    }

    "sum a positive number and a negative number bigger in absolute value" in {
      MixWord(1) + MixWord(0x400000002L) must be equalTo (false, MixWord(0x400000001L))
    }

    "sum a positive number and its negation to the positive zero" in {
      MixWord(1) + MixWord(0x400000001L) must be equalTo (false, MixWord(0))
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      MixWord(0x400000001L) + MixWord(1) must be equalTo (false, MixWord(0x400000000L))
    }
  }

  "decimal word subtraction" should {
    "subtract a positive number from a bigger positive number" in {
      MixWord(2) - MixWord(1) must be equalTo (false, MixWord(1))
    }

    "subtract a positive number from a smaller positive number" in {
      MixWord(1) - MixWord(2) must be equalTo (false, MixWord(0x400000001L))
    }

    "subtract a positive number from itself giving the positive zero" in {
      MixWord(1) - MixWord(1) must be equalTo (false, MixWord(0))
    }

    "subtract a negative number from itself giving the negative zero" in {
      MixWord(0x400000001L) - MixWord(0x400000001L) must be equalTo (false, MixWord(0x400000000L))
    }

    "subtract a negative number from a positive number" in {
      MixWord(1) - MixWord(0x400000002L) must be equalTo (false, MixWord(3))
    }

    "subtract a positive number from a negative number" in {
      MixWord(0x400000001L) - MixWord(2) must be equalTo (false, MixWord(0x400000003L))
    }

    "subtract a negative number from a positive number with an overflow" in {
      MixWord(5000000000L) - MixWord(0x400000000L + 5000000000L) must be equalTo (true, MixWord(0))
    }
  }

  "decimal word multiplication" should {
    "multiply two positive numbers" in {
      MixWord(2L) * MixWord(2L) must be equalTo MixDWord(0L, 4L)
    }

    "multiply two negative numbers" in {
      MixWord(0x400000002L) * MixWord(0x400000002L) must be equalTo MixDWord(0L, 0x4L)
    }

    "multiply a positive and a negative numbers" in {
      MixWord(2L) * MixWord(0x400000002L) must be equalTo MixDWord(0x400000000L, 4L)
    }

    "shift the result correctly" in {
      MixWord(1234567890L) * MixWord(0x400000000L | 987654321L) must be equalTo MixDWord(0x400000000L | 121932631L, 1126352690L)
    }

    "multiply the positive zero and a positive number the positive zero" in {
      MixWord(0L) * MixWord(1L) must be equalTo MixDWord(0L, 0L)
    }

    "multiply the positive zero and a negative number to the negative zero" in {
      MixWord(0L) * MixWord(0x400000001L) must be equalTo MixDWord(0x400000000L, 0L)
    }

    "multiply the negative zero and a positive number the negative zero" in {
      MixWord(0x400000000L) * MixWord(1L) must be equalTo MixDWord(0x400000000L, 0L)
    }

    "multiply the negative zero and a negative number to the positive zero" in {
      MixWord(0x400000000L) * MixWord(0x400000001L) must be equalTo MixDWord(0L, 0L)
    }
  }

  "decimal word division" should {
    "divide a positive number by a positive number" in {
      MixWord(15L) / MixWord(7L) must be equalTo (MixWord(2L), MixWord(1L))
    }

    "divide a positive number by a negative number" in {
      MixWord(15L) / MixWord(0x400000007L) must be equalTo (MixWord(0x400000002L), MixWord(1L))
    }

    "divide a negative number by a positive number" in {
      MixWord(0x400000000L | 15L) / MixWord(7L) must be equalTo (MixWord(0x400000002L), MixWord(0x400000001L))
    }

    "divide a negative number by a negative number" in {
      MixWord(0x400000000L | 15L) / MixWord(0x400000007L) must be equalTo (MixWord(2L), MixWord(0x400000001L))
    }

    "divide the positive zero by a positive number" in {
      MixWord(0L) / MixWord(1L) must be equalTo (MixWord(0L), MixWord(0L))
    }

    "divide the positive zero by a negative number" in {
      MixWord(0L) / MixWord(0x400000001L) must be equalTo (MixWord(0x400000000L), MixWord(0L))
    }

    "divide the negative zero by a positive number" in {
      MixWord(0x400000000L) / MixWord(1L) must be equalTo (MixWord(0x400000000L), MixWord(0x400000000L))
    }

    "divide the negative zero by a negative number" in {
      MixWord(0x400000000L) / MixWord(0x400000001L) must be equalTo (MixWord(0L), MixWord(0x400000000L))
    }

    "throw an exception on division by the positive zero" in {
      MixWord(1L) / MixWord(0L) must throwA[DivisionByZeroException]
    }

    "throw an exception on division by the negative zero" in {
      MixWord(1L) / MixWord(0x400000000L) must throwA[DivisionByZeroException]
    }
  }

  "decimal word division to the fractional part" should {
    "divide a positive number by a positive number" in {
      MixWord(2L) /\ MixWord(7L) must be equalTo (MixWord(2857142857L), MixWord(1L))
    }

    "divide a positive number by a negative number" in {
      MixWord(2L) /\ MixWord(0x400000007L) must be equalTo (MixWord(0x400000000L | 2857142857L), MixWord(1L))
    }

    "divide a negative number by a positive number" in {
      MixWord(0x400000002L) /\ MixWord(7L) must be equalTo (MixWord(0x400000000L | 2857142857L), MixWord(0x400000001L))
    }

    "divide a negative number by a negative number" in {
      MixWord(0x400000002L) /\ MixWord(0x400000007L) must be equalTo (MixWord(2857142857L), MixWord(0x400000001L))
    }

    "divide the positive zero by a positive number" in {
      MixWord(0L) /\ MixWord(1L) must be equalTo (MixWord(0L), MixWord(0L))
    }

    "divide the positive zero by a negative number" in {
      MixWord(0L) /\ MixWord(0x400000001L) must be equalTo (MixWord(0x400000000L), MixWord(0L))
    }

    "divide the negative zero by a positive number" in {
      MixWord(0x400000000L) /\ MixWord(1L) must be equalTo (MixWord(0x400000000L), MixWord(0x400000000L))
    }

    "divide the negative zero by a negative number" in {
      MixWord(0x400000000L) /\ MixWord(0x400000001L) must be equalTo (MixWord(0L), MixWord(0x400000000L))
    }

    "throw an exception on division by the positive zero" in {
      MixWord(1L) /\ MixWord(0L) must throwA[DivisionByZeroException]
    }

    "throw an exception on division by the negative zero" in {
      MixWord(1L) /\ MixWord(0x400000000L) must throwA[DivisionByZeroException]
    }

    "throw an exception if the dividend is too big" in {
      MixWord(7L) /\ MixWord(7L) must throwAn[OverflowException]
    }
  }

  "decimal double word division" should {
    "divide a positive number by a positive number" in {
      MixDWord(0L, 15L) / MixWord(7L) must be equalTo (MixWord(2L), MixWord(1L))
    }

    "divide a positive number by a negative number" in {
      MixDWord(0L, 15L) / MixWord(0x400000007L) must be equalTo (MixWord(0x400000002L), MixWord(1L))
    }

    "divide a negative number by a positive number" in {
      MixDWord(0x400000000L, 15L) / MixWord(7L) must be equalTo (MixWord(0x400000002L), MixWord(0x400000001L))
    }

    "divide a negative number by a negative number" in {
      MixDWord(0x400000000L, 15L) / MixWord(0x400000007L) must be equalTo (MixWord(2L), MixWord(0x400000001L))
    }

    "divide the positive zero by a positive number" in {
      MixDWord(0L, 0L) / MixWord(1L) must be equalTo (MixWord(0L), MixWord(0L))
    }

    "divide the positive zero by a negative number" in {
      MixDWord(0L, 0L) / MixWord(0x400000001L) must be equalTo (MixWord(0x400000000L), MixWord(0L))
    }

    "divide the negative zero by a positive number" in {
      MixDWord(0x400000000L, 0L) / MixWord(1L) must be equalTo (MixWord(0x400000000L), MixWord(0x400000000L))
    }

    "divide the negative zero by a negative number" in {
      MixDWord(0x400000000L, 0L) / MixWord(0x400000001L) must be equalTo (MixWord(0L), MixWord(0x400000000L))
    }

    "throw an exception on division by the positive zero" in {
      MixDWord(0L, 1L) / MixWord(0L) must throwA[DivisionByZeroException]
    }

    "throw an exception on division by the negative zero" in {
      MixDWord(0L, 1L) / MixWord(0x400000000L) must throwA[DivisionByZeroException]
    }

    "throw an exception if the dividend is too big" in {
      MixDWord(1L, 0L) / MixWord(0x1) must throwAn[OverflowException]
    }
  }

  "decimal index negation" should {
    "negate a positive number to a negative number" in {
      -MixIndex(1) must be equalTo MixIndex(0x4001)
    }

    "negate a negative number to a positive number" in {
      -MixIndex(0x4001) must be equalTo MixIndex(1)
    }

    "negate the positive zero to the negative zero" in {
      -MixIndex(0) must be equalTo MixIndex(0x4000)
    }

    "negate the negative zero to the positive zero" in {
      -MixIndex(0x4000) must be equalTo MixIndex(0)
    }
  }

  "decimal index addition" should {
    "sum two positive numbers" in {
      MixIndex(1) + MixIndex(2) must be equalTo MixIndex(3)
    }

    "sum two negative numbers" in {
      MixIndex(0x4001) + MixIndex(0x4002) must be equalTo MixIndex(0x4003)
    }

    "throw an exception if the sum is too big" in {
      MixIndex(5000) + MixIndex(5000) must throwAn[OverflowException]
    }

    "sum a positive number and a negative number smaller in absolute value" in {
      MixIndex(2) + MixIndex(0x4001) must be equalTo MixIndex(1)
    }

    "sum a positive number and a negative number bigger in absolute value" in {
      MixIndex(1) + MixIndex(0x4002) must be equalTo MixIndex(0x4001)
    }

    "sum a positive number and its negation to the positive zero" in {
      MixIndex(1) + MixIndex(0x4001) must be equalTo MixIndex(0)
    }

    "sum a negative number and its negation (positive) to the negative zero" in {
      MixIndex(0x4001) + MixIndex(1) must be equalTo MixIndex(0x4000)
    }
  }

  "decimal index offsetting" should {
    "add a positive offset to a positive index" in {
      MixIndex(1) + 2 must be equalTo MixIndex(3)
    }

    "add a positive offset to a negative index smaller in absolute value" in {
      MixIndex(0x4001) + 2 must be equalTo MixIndex(1)
    }

    "add a positive offset to a negative index bigger in absolute value" in {
      MixIndex(0x4002) + 1 must be equalTo MixIndex(0x4001)
    }

    "throw an exception if an offset is negative" in {
      MixIndex(1) + (-2) must throwAn[Error]
    }

    "throw an exception if the sum is too big" in {
      MixIndex(5000) + 5000 must throwAn[OverflowException]
    }

    "calculate the next index of a positive index" in {
      MixIndex(1).next must be equalTo MixIndex(2)
    }

    "throw an exception if the next index of a negative index is requested" in {
      MixIndex(0x4001).next must throwAn[Error]
    }

    "throw an exception if the next index is too big" in {
      MixIndex(9999).next must throwAn[OverflowException]
    }
  }

  "decimal index subtraction" should {
    "subtract a positive number from a bigger positive number" in {
      MixIndex(2) - MixIndex(1) must be equalTo MixIndex(1)
    }

    "subtract a positive number from a smaller positive number" in {
      MixIndex(1) - MixIndex(2) must be equalTo MixIndex(0x4001)
    }

    "subtract a positive number from itself giving the positive zero" in {
      MixIndex(1) - MixIndex(1) must be equalTo MixIndex(0)
    }

    "subtract a negative number from itself giving the negative zero" in {
      MixIndex(0x4001) - MixIndex(0x4001) must be equalTo MixIndex(0x4000)
    }

    "subtract a negative number from a positive number" in {
      MixIndex(1) - MixIndex(0x4002) must be equalTo MixIndex(3)
    }

    "subtract a positive number from a negative number" in {
      MixIndex(0x4001) - MixIndex(2) must be equalTo MixIndex(0x4003)
    }

    "subtract a negative number from a positive number with an overflow" in {
      MixIndex(5000) - MixIndex((0x4000 + 5000).toShort) must throwAn[OverflowException]
    }
  }

}
