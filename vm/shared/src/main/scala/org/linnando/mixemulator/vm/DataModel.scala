package org.linnando.mixemulator.vm

import org.linnando.mixemulator.vm.Comparison.Comparison
import org.linnando.mixemulator.vm.io.Device
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.immutable.Queue

trait DataModel {
  type RS <: AbstractRegisterState
  type MS <: AbstractMemoryState
  type B <: AbstractMixByte
  type I <: AbstractMixIndex
  type W <: AbstractMixWord
  type DW <: AbstractMixDWord

  def BYTE_SIZE: Byte

  case class State(registers: RS,
                   memory: MS,
                   programCounter: I,
                   timeCounter: Int,
                   isHalted: Boolean,
                   devices: Map[Int, (Device, Option[I])]) extends VirtualMachineState {
    override def get(address: Short): IOWord = memory.getCurrent(address).toIOWord

    override def getA: IOWord = registers.getA.toIOWord

    override def getX: IOWord = registers.getX.toIOWord

    override def getI1: IOWord = registers.getI(1).toIOWord

    override def getI2: IOWord = registers.getI(2).toIOWord

    override def getI3: IOWord = registers.getI(3).toIOWord

    override def getI4: IOWord = registers.getI(4).toIOWord

    override def getI5: IOWord = registers.getI(5).toIOWord

    override def getI6: IOWord = registers.getI(6).toIOWord

    override def getJ: IOWord = registers.getJ.toIOWord

    override def getOV: Boolean = registers.getOV

    override def getCMP: Comparison = registers.getCMP

    override def getProgramCounter: Short = programCounter.toShort

    override def getTimeCounter: Int = timeCounter

    override def getDevice(deviceNum: Int): Device = devices(deviceNum)._1
  }

  def getByte(value: Byte): B

  def getIndex(value: Short): I

  def getWord(value: Long): W

  def getWord(address: I, indexSpec: B, fieldSpec: B, opCode: B): W

  def getWord(ioWord: IOWord): W

  def getWord(chars: String): W

  trait AbstractRegisterState {
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

  trait AbstractMemoryState {
    def get(address: I): W

    def get(address: Short): W

    def getCurrent(address: Short): W

    def updated(address: I, value: W): MS

    def withSharedLock(address: I, size: Int, deviceNum: Int): MS

    def withExclusiveLock(address: I, size: Int, deviceNum: Int): MS

    def withoutLocks(deviceNum: Int): MS
  }

  trait AbstractMixByte {
    def toInt: Int

    def toByte: Byte

    def isZero: Boolean
  }

  trait AbstractMixIndex {
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

    def toIOWord: IOWord
  }

  trait AbstractMixWord {
    def isPositive: Boolean

    def isNegative: Boolean

    def getAddress: I

    def getIndexSpec: B

    def getFieldSpec: B

    def getOpCode: B

    def getField(fieldSpec: B): W

    def updated(fieldSpec: B, value: W): W

    def unary_-(): W

    def +(other: W): (Boolean, W)

    def -(subtrahend: W): (Boolean, W)

    def *(other: W): DW

    def /(divisor: W): (W, W)

    def /\(divisor: W): (W, W)

    def :*(other: W): W

    def <=>(other: W): Comparison

    def <<(n: I): W

    def >>(n: I): W

    def toByte: B

    def toIndex: I

    def toLong: Long

    def toIOWord: IOWord

    def toCharCode: DW
  }

  trait AbstractMixDWord {
    def isPositive: Boolean

    def isNegative: Boolean

    def /(divisor: W): (W, W)

    def <<(n: I): DW

    def >>(n: I): DW

    def <<|(n: I): DW

    def >>|(n: I): DW

    def charToNumber: W

    def toWord: W
  }

}
