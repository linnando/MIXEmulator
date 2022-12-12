package org.linnando.mixemulator

import org.linnando.mixemulator.vm.io.Device
import org.linnando.mixemulator.vm.io.data.IOWord
import org.linnando.mixemulator.vm.io.file._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DevicesFrontEnd")
class DevicesFrontEnd(blockAccessFileOps: BlockAccessFileOps,
                      lineAccessFileInputOps: LineAccessFileInputOps,
                      lineAccessFileOutputOps: LineAccessFileOutputOps) {
  def createDevices(): Map[Int, Device] = Map(
    0 -> FileTapeUnit.create("device0", blockAccessFileOps),
    1 -> FileTapeUnit.create("device1", blockAccessFileOps),
    2 -> FileTapeUnit.create("device2", blockAccessFileOps),
    3 -> FileTapeUnit.create("device3", blockAccessFileOps),
    4 -> FileTapeUnit.create("device4", blockAccessFileOps),
    5 -> FileTapeUnit.create("device5", blockAccessFileOps),
    6 -> FileTapeUnit.create("device6", blockAccessFileOps),
    7 -> FileTapeUnit.create("device7", blockAccessFileOps),
    8 -> FileDiskUnit.create("device8", blockAccessFileOps),
    9 -> FileDiskUnit.create("device9", blockAccessFileOps),
    10 -> FileDiskUnit.create("device10", blockAccessFileOps),
    11 -> FileDiskUnit.create("device11", blockAccessFileOps),
    12 -> FileDiskUnit.create("device12", blockAccessFileOps),
    13 -> FileDiskUnit.create("device13", blockAccessFileOps),
    14 -> FileDiskUnit.create("device14", blockAccessFileOps),
    15 -> FileDiskUnit.create("device15", blockAccessFileOps),
    16 -> FileCardReader.create("device16", lineAccessFileInputOps),
    17 -> FileCardPunch.create("device17", lineAccessFileOutputOps),
    18 -> FileLinePrinter.create("device18", lineAccessFileOutputOps),
    20 -> FilePaperTape.create("device20", lineAccessFileInputOps)
  )

  def getBlockDeviceData(deviceNum: Int): Future[IndexedSeq[IOWord]] = deviceNum match {
    case i if i >= 0 && i < 16 =>
      blockAccessFileOps.getCurrentData(s"device$i").map(FileBlockIODevice.bytesToIOWords)
    case _ => throw new Error
  }

  @JSExport("getBlockDeviceData")
  def getBlockDeviceDataJs(deviceNum: Int): js.Promise[js.Array[IOWord]] =
    getBlockDeviceData(deviceNum).map(_.toJSArray).toJSPromise

  def saveBlockDevice(deviceNum: Int, data: Array[Byte]): Future[Unit] = {
    val device = deviceNum match {
      case i if i >= 0 && i < 8 => FileTapeUnit.create(s"device$i", data, blockAccessFileOps)
      case i if i >= 8 && i < 16 => FileDiskUnit.create(s"device$i", data, blockAccessFileOps)
      case _ => throw new Error
    }
    device.task.map(_ => ())
  }

  @JSExport("saveBlockDevice")
  def saveBlockDeviceJs(deviceNum: Int, data: js.Array[Byte]): js.Promise[Unit] =
    saveBlockDevice(deviceNum, data.toArray).toJSPromise

  def getLineDeviceData(deviceNum: Int): Future[IndexedSeq[String]] = deviceNum match {
    case 16 => lineAccessFileInputOps.getData("device16").map(_.split("\n"))
    case 17 => lineAccessFileOutputOps.getCurrentData("device17").map(_.split("\n"))
    case 18 => lineAccessFileOutputOps.getCurrentData("device18").map(_.split("\n"))
    case 20 => lineAccessFileInputOps.getData("device20").map(_.split("\n"))
    case _ => throw new Error
  }

  @JSExport("getLineDeviceData")
  def getLineDeviceDataJs(deviceNum: Int): js.Promise[js.Array[String]] =
    getLineDeviceData(deviceNum).map(_.toJSArray).toJSPromise

  def saveLineDevice(deviceNum: Int, data: String): Future[Unit] = {
    val device = deviceNum match {
      case 16 => FileCardReader.create("device16", data, lineAccessFileInputOps)
      case 20 => FilePaperTape.create("device20", data, lineAccessFileInputOps)
      case _ => throw new Error
    }
    device.task.map(_ => ())
  }

  @JSExport("saveLineDevice")
  def saveLineDeviceJs(deviceNum: Int, data: String): js.Promise[Unit] =
    saveLineDevice(deviceNum, data).toJSPromise
}

@JSExportTopLevel("DevicesFrontEnd$")
object DevicesFrontEnd {
  @JSExport("create")
  def createJs(blockAccessFileOps: BlockAccessFileOpsJs,
               lineAccessFileInputOps: LineAccessFileInputOpsJs,
               lineAccessFileOutputOps: LineAccessFileOutputOpsJs): DevicesFrontEnd =
    new DevicesFrontEnd(BlockAccessFileOps.createJs(blockAccessFileOps),
      LineAccessFileInputOps.createJs(lineAccessFileInputOps),
      LineAccessFileOutputOps.createJs(lineAccessFileOutputOps))
}
