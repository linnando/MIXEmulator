package org.linnando.mixemulator.webapp

import io.scalajs.nodejs.fs.{FileInputOptions, Fs}
import org.linnando.mixemulator.vm.exceptions.EndOfFileException
import org.linnando.mixemulator.vm.io.file.LineAccessFileInputOps

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class LineAccessFileInputOpsImpl extends LineAccessFileInputOps {
  override def readLine(filename: String, position: Long): Future[Array[Char]] =
    Fs.readFileFuture(filename, new FileInputOptions(encoding = "utf8")).map(data => {
      val lines = data.asInstanceOf[String].split("\n")
      if (position < lines.length) lines(position.toInt).toCharArray
      else throw new EndOfFileException
    })

  override def initialise(filename: String): Future[Unit] =
    for {
      exists <- Fs.existsFuture(filename)
      _ <- if (exists) Future {} else save(filename, "")
    } yield ()

  override def save(filename: String, data: String): Future[Unit] =
    Fs.writeFileFuture(s"/$filename", data)

  override def getData(filename: String): Future[String] =
    Fs.readFileFuture(s"/$filename", new FileInputOptions(encoding = "utf8")).map(_.asInstanceOf[String])
}
