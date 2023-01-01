package org.linnando.mixemulator.vm.io.file

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

class BlockAccessFileOps {
  def readBlock(filename: String, version: Int, position: Long, blockSize: Int): Future[Array[Byte]] = ???

  def writeBlock(filename: String, version: Int, position: Long, bytes: Array[Byte]): Future[Unit] = ???

  def initialiseWithCurrentVersion(filename: String): Future[Unit] = ???

  def getVersions(filename: String): Future[Iterable[String]] = ???

  def save(filename: String, data: Array[Byte]): Future[Unit] = ???

  def getCurrentData(filename: String): Future[Array[Byte]] = for {
    versions: Iterable[String] <- getVersions(filename)
    versionNumbers = versions.map(_.toInt)
    bytes: Array[Byte] <-
      if (versionNumbers.isEmpty) Future.successful(Array.empty[Byte])
      else getData(filename, versionNumbers.max)
  } yield bytes

  def getData(filename: String, version: Int): Future[Array[Byte]] = ???
}

@JSExportTopLevel("BlockAccessFileOps")
class BlockAccessFileOpsJs extends js.Object {
  def readBlock(filename: String, version: Int, position: Double, blockSize: Int): js.Promise[js.Array[Byte]] = ???

  def writeBlock(filename: String, version: Int, position: Double, bytes: js.Array[Byte]): js.Promise[Unit] = ???

  def initialiseWithCurrentVersion(filename: String): js.Promise[Unit] = ???

  def getVersions(filename: String): js.Promise[js.Array[String]] = ???

  def save(filename: String, data: js.Array[Byte]): js.Promise[Unit] = ???

  def getData(filename: String, version: Int): js.Promise[js.Array[Byte]] = ???
}

object BlockAccessFileOps {
  def create(): BlockAccessFileOps = new BlockAccessFileOps {
    override def readBlock(filename: String, version: Int, position: Long, blockSize: Int): Future[Array[Byte]] = Future {
      val buffer = ByteBuffer.allocate(blockSize)
      val file = new FileInputStream(s"$filename/$version").getChannel
      try {
        file.read(buffer, position)
        if (buffer.hasArray) buffer.array()
        else {
          val bytes = new Array[Byte](blockSize)
          buffer.get(bytes)
          bytes
        }
      }
      finally {
        file.close()
      }
    }

    override def writeBlock(filename: String, version: Int, position: Long, bytes: Array[Byte]): Future[Unit] = Future {
      val oldFile = new File(s"$filename/$version")
      val channel = new FileOutputStream(s"$filename/${version + 1}").getChannel
      try {
        if (oldFile.exists) {
          val transferred = copyFile(oldFile, channel, 0, position)
          fillZeroes(channel, position - transferred, bytes.length)
        } else fillZeroes(channel, position, bytes.length)
        val buffer = ByteBuffer.wrap(bytes)
        channel.write(buffer)
        if (oldFile.exists)
          copyFile(oldFile, channel, position + bytes.length, Long.MaxValue)
      }
      finally {
        channel.close()
      }
    }

    private def copyFile(source: File, destChannel: FileChannel, position: Long, count: Long): Long = {
      val srcChannel = new FileInputStream(source).getChannel
      val transferred = srcChannel.transferTo(position, count, destChannel)
      srcChannel.close()
      transferred
    }

    private def fillZeroes(destChannel: FileChannel, position: Long, blockSize: Int): Unit = {
      val buffer = ByteBuffer.wrap(new Array[Byte](blockSize))
      (0 until (position / blockSize).toInt) foreach { _ => destChannel.write(buffer) }
      destChannel.write(ByteBuffer.wrap(new Array[Byte]((position % blockSize).toInt)))
    }

    override def initialiseWithCurrentVersion(filename: String): Future[Unit] = Future {
      val directory = new File(filename)
      directory.mkdirs()
      val versions = directory.listFiles()
      if (versions.isEmpty) {
        val channel = new FileOutputStream(s"$filename/0").getChannel
        channel.write(ByteBuffer.allocate(0))
      } else {
        val currentVersion = versions.map(_.getName.toInt).max
        if (currentVersion != 0)
          new File(s"/$filename/$currentVersion").renameTo(new File(s"/$filename/0"))
        versions.filter(version => version.getName != "0" && version.getName != currentVersion.toString)
          .foreach(_.delete())
      }
    }

    override def getVersions(filename: String): Future[Iterable[String]] = Future {
      val directory = new File(filename)
      directory.mkdirs()
      directory.listFiles().map(_.getName)
    }

    override def save(filename: String, data: Array[Byte]): Future[Unit] = ???

    override def getData(filename: String, version: Int): Future[Array[Byte]] = ???
  }

  def createJs(implementation: BlockAccessFileOpsJs): BlockAccessFileOps = new BlockAccessFileOps {
    override def readBlock(filename: String, version: Int, position: Long, blockSize: Int): Future[Array[Byte]] =
      implementation.readBlock(filename, version, position.toDouble, blockSize).toFuture.map(_.toArray)

    override def writeBlock(filename: String, version: Int, position: Long, bytes: Array[Byte]): Future[Unit] =
      implementation.writeBlock(filename, version, position.toDouble, bytes.toJSArray).toFuture

    override def initialiseWithCurrentVersion(filename: String): Future[Unit] =
      implementation.initialiseWithCurrentVersion(filename).toFuture

    override def getVersions(filename: String): Future[Iterable[String]] =
      implementation.getVersions(filename).toFuture.map(_.toIterable)

    override def save(filename: String, data: Array[Byte]): Future[Unit] =
      implementation.save(filename, data.toJSArray).toFuture

    override def getData(filename: String, version: Int): Future[Array[Byte]] =
      implementation.getData(filename, version).toFuture.map(_.toArray)
  }
}
