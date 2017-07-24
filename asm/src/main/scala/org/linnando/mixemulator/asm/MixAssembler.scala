package org.linnando.mixemulator.asm

import org.linnando.mixemulator.asm.exceptions._
import org.linnando.mixemulator.vm.{VirtualMachine, VirtualMachineBuilder}

import scala.util.matching.Regex

case class MixAssembler(builder: VirtualMachineBuilder, counterHistory: List[List[Short]]) {
  def parseLine(line: String): MixAssembler =
    line match {
      case MixAssembler.commentPattern(_*) =>
        sequential(builder)
      case MixAssembler.opLinePattern(label, op, rest) =>
        withOpLine(label, op, rest)
      case _ => throw new WrongLineException
    }

  private def sequential(nextBuilderState: VirtualMachineBuilder) = {
    copy(builder = nextBuilderState,
      counterHistory = (nextBuilderState.getCounter :: counterHistory.head) :: counterHistory.tail)
  }

  private def withOpLine(label: String, op: String, rest: String) = op match {
    case "EQU" => withEqu(label, rest)
    case "ORIG" => withOrig(label, rest)
    case "CON" => withCon(label, rest)
    case "ALF" => withAlf(label, rest)
    case "END" => withEnd(label, rest)
    case _ => withCommand(label, op, rest)
  }

  private def withEqu(label: String, addressPart: String) = addressPart match {
    case MixAssembler.addressPartWithWValue(wValue, _) =>
      sequential(builder.withWValueSymbol(label, wValue))
    case _ => throw new WrongAddressPartException(addressPart)
  }

  private def withOrig(label: String, addressPart: String) = addressPart match {
    case MixAssembler.addressPartWithWValue(wValue, _) =>
      val nextBuilderState = builder.withCurrentCounterSymbol(label).withOrig(wValue)
      copy(builder = nextBuilderState,
        counterHistory = List(nextBuilderState.getCounter) :: counterHistory)
    case _ => throw new WrongAddressPartException(addressPart)
  }

  private def withCon(label: String, addressPart: String) = addressPart match {
    case MixAssembler.addressPartWithWValue(wValue, _) =>
      sequential(builder.withCurrentCounterSymbol(label).withConstant(wValue))
    case _ => throw new WrongAddressPartException(addressPart)
  }

  private def withAlf(label: String, addressPart: String) = addressPart match {
    case MixAssembler.alfConstantPartWithoutSpace(chars, _) =>
      sequential(builder.withCurrentCounterSymbol(label).withCharCode(chars))
    case MixAssembler.alfConstantPartWithSpace(chars, _) =>
      sequential(builder.withCurrentCounterSymbol(label).withCharCode(chars))
    case _ => throw new WrongAddressPartException(addressPart)
  }

  private def withEnd(label: String, addressPart: String) = addressPart match {
    case MixAssembler.addressPartWithWValue(wValue, _) =>
      sequential(builder.withFinalSection(label, wValue))
    case _ => throw new WrongAddressPartException(addressPart)
  }

  private def withCommand(label: String, operator: String, addressPart: String) = {
    if (!MixAssembler.commands.contains(operator))
      throw new WrongOperatorException
    addressPart match {
      case MixAssembler.addressPartWithOpAddress(aPart, _, indexPart, _, fPart, _) =>
        val command = MixAssembler.commands(operator)
        if (fPart != null && !command._3) throw new FixedFieldSpecException
        else sequential(builder.withCurrentCounterSymbol(label)
          .withCommand(aPart, indexPart, fPart, command._1, command._2))
      case _ => throw new WrongAddressPartException(addressPart)
    }
  }
}

object MixAssembler {
  def translate(builder: VirtualMachineBuilder, lines: Seq[String]): (VirtualMachine, List[List[Short]]) = {
    val translated = lines.foldLeft(MixAssembler(builder, List(List(0)))) { (asm, line) =>
      asm.parseLine(line)
    }
    (translated.builder.build, translated.counterHistory)
  }

  val commentPattern: Regex = raw" *\*.*".r

  val opLinePattern: Regex = raw"([A-Z0-9]{0,10}) +([A-Z0-9]{1,4}) (.*)".r

  val addressPartWithWValue: Regex = raw" *([^ ]+)( .*)?".r

  val addressPartWithOpAddress: Regex = raw" *([^ ,(]*)(,([^( ]+))?(\(([^ ]+)\))?( .*)?".r

  val alfConstantPartWithoutSpace: Regex = raw"([^ ].{4})( .*)?".r

  val alfConstantPartWithSpace: Regex = raw" (.{5})( .*)?".r

  val commands: Map[String, (Byte, Byte, Boolean)] = Map(
    "NOP" -> (0.toByte, 0.toByte, true),
    "ADD" -> (1.toByte, 5.toByte, true),
    "SUB" -> (2.toByte, 5.toByte, true),
    "MUL" -> (3.toByte, 5.toByte, true),
    "DIV" -> (4.toByte, 5.toByte, true),
    "NUM" -> (5.toByte, 0.toByte, false),
    "CHAR" -> (5.toByte, 1.toByte, false),
    "HLT" -> (5.toByte, 2.toByte, false),
    "SLA" -> (6.toByte, 0.toByte, false),
    "SRA" -> (6.toByte, 1.toByte, false),
    "SLAX" -> (6.toByte, 2.toByte, false),
    "SRAX" -> (6.toByte, 3.toByte, false),
    "SLC" -> (6.toByte, 4.toByte, false),
    "SRC" -> (6.toByte, 5.toByte, false),
    "MOVE" -> (7.toByte, 1.toByte, true),
    "LDA" -> (8.toByte, 5.toByte, true),
    "LD1" -> (9.toByte, 5.toByte, true),
    "LD2" -> (10.toByte, 5.toByte, true),
    "LD3" -> (11.toByte, 5.toByte, true),
    "LD4" -> (12.toByte, 5.toByte, true),
    "LD5" -> (13.toByte, 5.toByte, true),
    "LD6" -> (14.toByte, 5.toByte, true),
    "LDX" -> (15.toByte, 5.toByte, true),
    "LDAN" -> (16.toByte, 5.toByte, true),
    "LD1N" -> (17.toByte, 5.toByte, true),
    "LD2N" -> (18.toByte, 5.toByte, true),
    "LD3N" -> (19.toByte, 5.toByte, true),
    "LD4N" -> (20.toByte, 5.toByte, true),
    "LD5N" -> (21.toByte, 5.toByte, true),
    "LD6N" -> (22.toByte, 5.toByte, true),
    "LDXN" -> (23.toByte, 5.toByte, true),
    "STA" -> (24.toByte, 5.toByte, true),
    "ST1" -> (25.toByte, 5.toByte, true),
    "ST2" -> (26.toByte, 5.toByte, true),
    "ST3" -> (27.toByte, 5.toByte, true),
    "ST4" -> (28.toByte, 5.toByte, true),
    "ST5" -> (29.toByte, 5.toByte, true),
    "ST6" -> (30.toByte, 5.toByte, true),
    "STX" -> (31.toByte, 5.toByte, true),
    "STJ" -> (32.toByte, 2.toByte, true),
    "STZ" -> (33.toByte, 5.toByte, true),
    "JBUS" -> (34.toByte, 0.toByte, true),
    "IOC" -> (35.toByte, 0.toByte, true),
    "IN" -> (36.toByte, 0.toByte, true),
    "OUT" -> (37.toByte, 0.toByte, true),
    "JRED" -> (38.toByte, 0.toByte, true),
    "JMP" -> (39.toByte, 0.toByte, false),
    "JSJ" -> (39.toByte, 1.toByte, false),
    "JOV" -> (39.toByte, 2.toByte, false),
    "JNOV" -> (39.toByte, 3.toByte, false),
    "JL" -> (39.toByte, 4.toByte, false),
    "JE" -> (39.toByte, 5.toByte, false),
    "JG" -> (39.toByte, 6.toByte, false),
    "JGE" -> (39.toByte, 7.toByte, false),
    "JNE" -> (39.toByte, 8.toByte, false),
    "JLE" -> (39.toByte, 9.toByte, false),
    "JAN" -> (40.toByte, 0.toByte, false),
    "JAZ" -> (40.toByte, 1.toByte, false),
    "JAP" -> (40.toByte, 2.toByte, false),
    "JANN" -> (40.toByte, 3.toByte, false),
    "JANZ" -> (40.toByte, 4.toByte, false),
    "JANP" -> (40.toByte, 5.toByte, false),
    "J1N" -> (41.toByte, 0.toByte, false),
    "J1Z" -> (41.toByte, 1.toByte, false),
    "J1P" -> (41.toByte, 2.toByte, false),
    "J1NN" -> (41.toByte, 3.toByte, false),
    "J1NZ" -> (41.toByte, 4.toByte, false),
    "J1NP" -> (41.toByte, 5.toByte, false),
    "J2N" -> (42.toByte, 0.toByte, false),
    "J2Z" -> (42.toByte, 1.toByte, false),
    "J2P" -> (42.toByte, 2.toByte, false),
    "J2NN" -> (42.toByte, 3.toByte, false),
    "J2NZ" -> (42.toByte, 4.toByte, false),
    "J2NP" -> (42.toByte, 5.toByte, false),
    "J3N" -> (43.toByte, 0.toByte, false),
    "J3Z" -> (43.toByte, 1.toByte, false),
    "J3P" -> (43.toByte, 2.toByte, false),
    "J3NN" -> (43.toByte, 3.toByte, false),
    "J3NZ" -> (43.toByte, 4.toByte, false),
    "J3NP" -> (43.toByte, 5.toByte, false),
    "J4N" -> (44.toByte, 0.toByte, false),
    "J4Z" -> (44.toByte, 1.toByte, false),
    "J4P" -> (44.toByte, 2.toByte, false),
    "J4NN" -> (44.toByte, 3.toByte, false),
    "J4NZ" -> (44.toByte, 4.toByte, false),
    "J4NP" -> (44.toByte, 5.toByte, false),
    "J5N" -> (45.toByte, 0.toByte, false),
    "J5Z" -> (45.toByte, 1.toByte, false),
    "J5P" -> (45.toByte, 2.toByte, false),
    "J5NN" -> (45.toByte, 3.toByte, false),
    "J5NZ" -> (45.toByte, 4.toByte, false),
    "J5NP" -> (45.toByte, 5.toByte, false),
    "J6N" -> (46.toByte, 0.toByte, false),
    "J6Z" -> (46.toByte, 1.toByte, false),
    "J6P" -> (46.toByte, 2.toByte, false),
    "J6NN" -> (46.toByte, 3.toByte, false),
    "J6NZ" -> (46.toByte, 4.toByte, false),
    "J6NP" -> (46.toByte, 5.toByte, false),
    "JXN" -> (47.toByte, 0.toByte, false),
    "JXZ" -> (47.toByte, 1.toByte, false),
    "JXP" -> (47.toByte, 2.toByte, false),
    "JXNN" -> (47.toByte, 3.toByte, false),
    "JXNZ" -> (47.toByte, 4.toByte, false),
    "JXNP" -> (47.toByte, 5.toByte, false),
    "INCA" -> (48.toByte, 0.toByte, false),
    "DECA" -> (48.toByte, 1.toByte, false),
    "ENTA" -> (48.toByte, 2.toByte, false),
    "ENNA" -> (48.toByte, 3.toByte, false),
    "INC1" -> (49.toByte, 0.toByte, false),
    "DEC1" -> (49.toByte, 1.toByte, false),
    "ENT1" -> (49.toByte, 2.toByte, false),
    "ENN1" -> (49.toByte, 3.toByte, false),
    "INC2" -> (50.toByte, 0.toByte, false),
    "DEC2" -> (50.toByte, 1.toByte, false),
    "ENT2" -> (50.toByte, 2.toByte, false),
    "ENN2" -> (50.toByte, 3.toByte, false),
    "INC3" -> (51.toByte, 0.toByte, false),
    "DEC3" -> (51.toByte, 1.toByte, false),
    "ENT3" -> (51.toByte, 2.toByte, false),
    "ENN3" -> (51.toByte, 3.toByte, false),
    "INC4" -> (52.toByte, 0.toByte, false),
    "DEC4" -> (52.toByte, 1.toByte, false),
    "ENT4" -> (52.toByte, 2.toByte, false),
    "ENN4" -> (52.toByte, 3.toByte, false),
    "INC5" -> (53.toByte, 0.toByte, false),
    "DEC5" -> (53.toByte, 1.toByte, false),
    "ENT5" -> (53.toByte, 2.toByte, false),
    "ENN5" -> (53.toByte, 3.toByte, false),
    "INC6" -> (54.toByte, 0.toByte, false),
    "DEC6" -> (54.toByte, 1.toByte, false),
    "ENT6" -> (54.toByte, 2.toByte, false),
    "ENN6" -> (54.toByte, 3.toByte, false),
    "INCX" -> (55.toByte, 0.toByte, false),
    "DECX" -> (55.toByte, 1.toByte, false),
    "ENTX" -> (55.toByte, 2.toByte, false),
    "ENNX" -> (55.toByte, 3.toByte, false),
    "CMPA" -> (56.toByte, 5.toByte, true),
    "CMP1" -> (57.toByte, 5.toByte, true),
    "CMP2" -> (58.toByte, 5.toByte, true),
    "CMP3" -> (59.toByte, 5.toByte, true),
    "CMP4" -> (60.toByte, 5.toByte, true),
    "CMP5" -> (61.toByte, 5.toByte, true),
    "CMP6" -> (62.toByte, 5.toByte, true),
    "CMPX" -> (63.toByte, 5.toByte, true)
  )
}
