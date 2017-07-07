package org.linnando.mixemulator.vm

import Comparison.Comparison
import org.linnando.mixemulator.vm.io.Device
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue

trait DataModel {
  type RS <: RegisterState
  type MS <: MemoryState
  type B <: MixByte
  type I <: MixIndex
  type W <: MixWord
  type DW <: MixDWord

  case class State(registers: RS,
                   memory: MS,
                   programCounter: I,
                   timeCounter: Int,
                   devices: IndexedSeq[(Device, Queue[I])])

  def getZero: W

  def getWord(ioWord: IOWord): W

  trait RegisterState {
    def getA: W

    def getX: W

    def getAX: DW

    def getI(index: B): I

    def getI(index: Int): I

    def getJ: I

    def getOV: Boolean

    def getCMP: Comparison

    def updatedA(value: W): RS

    def updatedX(value: W): RS

    def updatedAX(value: DW, xIsNegative: Boolean): RS

    def updatedI(index: B, value: I): RS

    def updatedI(index: Int, value: I): RS

    def updatedJ(value: I): RS

    def updatedOV(value: Boolean): RS

    def updatedCMP(value: Comparison): RS
  }

  trait MemoryState {
    def get(address: I): W

    def updated(address: I, value: W): MS

    def updated(address: I, fieldSpec: B, value: W): MS

    def withSharedLock(address: I, size: Int, deviceNum: Int): MS

    def withExclusiveLock(address: I, size: Int, deviceNum: Int): MS

    def withoutLocks(deviceNum: Int): MS
  }

  trait MixByte {
    def toInt: Int

    def toByte: Byte

    def isZero: Boolean
  }

  trait MixIndex {
    def isPositive: Boolean

    def isNegative: Boolean

    def unary_-(): I

    def +(other: I): I

    def +(other: Int): I

    def -(subtrahend: I): I

    def <=>(other: W): Comparison

    def next: I

    def toShort: Short

    def toWord: W
  }

  trait MixWord {
    def isPositive: Boolean

    def isNegative: Boolean

    def getAddress: I

    def getIndexSpec: B

    def getFieldSpec: B

    def getOpCode: B

    def getField(fieldSpec: B): W

    def unary_-(): W

    def +(other: W): (Boolean, W)

    def -(subtrahend: W): (Boolean, W)

    def *(other: W): DW

    def <=>(other: W): Comparison

    def <<(n: I): W

    def >>(n: I): W

    def toIndex: I

    def toLong: Long

    def toIOWord: IOWord

    def toCharCode: DW
  }

  trait MixDWord {
    def isPositive: Boolean

    def isNegative: Boolean

    def /(divisor: W): (W, W)

    def <<(n: I): DW

    def >>(n: I): DW

    def <<|(n: I): DW

    def >>|(n: I): DW

    def charToNumber: W
  }

}
