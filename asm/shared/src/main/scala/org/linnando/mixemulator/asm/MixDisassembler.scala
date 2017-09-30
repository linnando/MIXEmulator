package org.linnando.mixemulator.asm

import org.linnando.mixemulator.vm.ProcessingModel
import org.linnando.mixemulator.vm.io.data.IOWord

class MixDisassembler(model: ProcessingModel) {
  private val BYTE_SIZE = model.BYTE_SIZE

  def disassembleLine(ioWord: IOWord): String = {
    val Seq(byte0, byte1, byte2, byte3, byte4) = ioWord.bytes
    val sign = if (ioWord.negative) "-" else ""
    val addressPart = (byte0 * BYTE_SIZE + byte1).toString
    val indexPart = if (byte2 > 0) s",$byte2" else ""
    val (operator, fieldSpec) = MixDisassembler.commands(byte4)(byte3)
    s"           $operator $sign$addressPart$indexPart$fieldSpec"
  }
}

object MixDisassembler {
  val commands: Byte => Map[Byte, (String, String)] = {
    val initialMap = Map.empty[Byte, Map[Byte, (String, String)]] withDefaultValue Map.empty[Byte, (String, String)]
    MixAssembler.commands.foldLeft(initialMap) { (map, command) =>
      val operator = f"${command._1}%-4s"
      val updatedInnerMap =
        if (command._2._3) Map(command._2._2 -> (operator, "")) withDefault { fieldSpec =>
          (operator, s"(${fieldSpec / 8}:${fieldSpec % 8})")
        }
        else map(command._2._1).updated(command._2._2, (operator, ""))
      map.updated(command._2._1, updatedInnerMap)
    }
  }
}
