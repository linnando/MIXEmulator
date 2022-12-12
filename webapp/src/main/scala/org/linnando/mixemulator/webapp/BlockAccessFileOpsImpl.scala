package org.linnando.mixemulator.webapp

import io.scalajs.nodejs.buffer.Buffer
import io.scalajs.nodejs.fs.Fs
import org.linnando.mixemulator.vm.io.file.BlockAccessFileOps

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

class BlockAccessFileOpsImpl extends BlockAccessFileOps {
  override def readBlock(filename: String, version: Int, position: Long, blockSize: Int): Future[Array[Byte]] = {
    val buffer = Buffer.alloc(blockSize)
    for {
      fd <- Fs.openFuture(s"$filename/$version", "r")
      _ <- Fs.readFuture(fd, buffer, 0, blockSize, position.toInt)
    } yield buffer.asInstanceOf[Uint8Array].toArray.map(_.toByte)
  }

  override def writeBlock(filename: String, version: Int, position: Long, bytes: Array[Byte]): Future[Unit] = {
    val oldFile = s"$filename/$version"
    val newFile = s"$filename/${version + 1}"
    for {
      exists <- Fs.existsFuture(oldFile)
      oldFileSize <- if (exists) copyFileFuture(oldFile, newFile) else Future(0)
      _ <- if (oldFileSize < position) appendZeroes(newFile, (position - oldFileSize).toInt) else Future {}
      _ <- replaceBlock(newFile, bytes, position.toInt)
    } yield ()
  }

  // TODO Implement without reading the whole file to memory
  private def copyFileFuture(src: String, dest: String): Future[Int] =
    for {
      buffer <- Fs.readFileFuture(src).map(_.asInstanceOf[Buffer])
      _ <- Fs.writeFileFuture(dest, buffer)
    } yield buffer.length

  private def appendZeroes(dest: String, count: Int): Future[Unit] =
    Fs.appendFileFuture(dest, Buffer.alloc(count))

  private def replaceBlock(file: String, bytes: Array[Byte], position: Int): Future[Unit] = {
    val buffer = Buffer.from(new js.Array[Int](0) ++ bytes.map(_.toInt))
    // TODO This is not going to work under Node.js on Linux since the kernel ignores position
    // in the append mode. However, as long as we only use BrowserFS, it seems to work
    Fs.openFuture(file, "a").flatMap(dest =>
      Fs.writeFuture(dest, buffer, 0, bytes.length, position).map(_ => ()) andThen {
        case _ => Fs.closeFuture(dest)
      }
    )
  }

  override def initialiseWithCurrentVersion(filename: String): Future[Unit] =
    for {
      _ <- ensureDeviceDirectoryExists(filename)
      versions <- getVersions(filename)
      _ <- saveInitialVersionOnly(filename, versions)
    } yield ()

  override def getVersions(filename: String): Future[Iterable[String]] =
    Fs.readdirFuture(s"/$filename").map(_.toArray[String])

  private def ensureDeviceDirectoryExists(filename: String): Future[Unit] =
    for {
      exists <- Fs.existsFuture(s"/$filename")
      isDirectory <- if (exists) Fs.statFuture(s"/$filename").map(_.isDirectory()) else Future(false)
      _ <- if (exists && !isDirectory) Fs.unlinkFuture(s"/$filename") else Future {}
      _ <- if (exists && isDirectory) Future {} else Fs.mkdirFuture(s"/$filename")
    } yield ()

  private def saveInitialVersionOnly(filename: String, versions: Iterable[String]): Future[Unit] =
    if (versions.isEmpty) {
      val buffer = Buffer.alloc(0)
      Fs.writeFileFuture(s"/$filename/0", buffer)
    } else {
      val currentVersion = versions.map(_.toInt).max
      for {
        _ <- if (currentVersion != 0) Fs.renameFuture(s"/$filename/$currentVersion", s"/$filename/0")
        else Future.successful(())
        _ <- unlinkVersions(filename, versions.filter(version => version != "0" && version != currentVersion.toString))
      } yield ()
    }

  private def unlinkVersions(filename: String, versions: Iterable[String]): Future[Unit] = {
    val futures = versions.map(version => Fs.unlinkFuture(s"/$filename/$version"))
    Future.fold(futures)(())((_, _) => ())
  }

  override def save(filename: String, data: Array[Byte]): Future[Unit] = {
    val buffer = Buffer.from(new js.Array[Int](0) ++ data.map(_.toInt))
    Fs.writeFileFuture(s"/$filename/0", buffer)
  }

  override def getData(filename: String, version: Int): Future[Array[Byte]] =
    Fs.readFileFuture(s"/$filename/$version").map(_.asInstanceOf[Uint8Array].toArray.map(_.toByte))
}
