package org.linnando.mixemulator.vm.io

trait CardPunch extends LineDevice with PositionalOutputDevice {
}

object CardPunch {
  val BLOCK_SIZE = 16
}
