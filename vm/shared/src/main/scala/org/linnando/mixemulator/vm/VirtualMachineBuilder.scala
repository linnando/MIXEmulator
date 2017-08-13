package org.linnando.mixemulator.vm

import scala.util.matching.Regex

trait VirtualMachineBuilder {
  def getCounter: Short

  def withWValueSymbol(label: String, wValue: String): VirtualMachineBuilder

  def withCurrentCounterSymbol(label: String): VirtualMachineBuilder

  def withOrig(wValue: String): VirtualMachineBuilder

  def withConstant(wValue: String): VirtualMachineBuilder

  def withCharCode(chars: String): VirtualMachineBuilder

  def withFinalSection(label: String, value: String): VirtualMachineBuilder

  def withCommand(aPart: String, indexPart: String, fPart: String, opCode: Byte, defaultFieldSpec: Byte): VirtualMachineBuilder

  def build: VirtualMachine

  def buildTracking: TrackingVirtualMachine
}

object VirtualMachineBuilder {
  val localLabel: Regex = raw"[0-9]H".r

  val localBackReference: Regex = raw"([0-9])B".r

  val localForwardReference: Regex = raw"([0-9])F".r

  val expressionAndFieldSpec: Regex = raw"([^(]+)(\((.+)\))?".r

  val signedElementaryExpression: Regex = raw"(\+|-)?([A-Z0-9]{1,10}|\*)".r

  val expressionSegment: Regex = raw"(\+|-|\*|/|//|:)([A-Z0-9]{1,10}|\*)".r

  val number: Regex = raw"[0-9]{1,10}".r

  val numberOrSymbol: Regex = raw"[A-Z0-9]{1,10}".r

  val literal: Regex = raw"=(.*)=".r
}
