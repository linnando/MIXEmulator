package org.linnando.mixemulator.vm.io.file

import org.linnando.mixemulator.vm.exceptions.EndOfFileException

import java.io._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

trait LineAccessFileInputOps {
  def readLine(filename: String, position: Long): Future[Array[Char]]

  def initialise(filename: String): Future[Unit]

  def save(filename: String, contents: String): Future[Unit]

  def getData(filename: String): Future[String]
}

@JSExportTopLevel("LineAccessFileInputOps")
class LineAccessFileInputOpsJs extends js.Object {
  def readLine(filename: String, position: Double): js.Promise[String] = ???

  def initialise(filename: String): js.Promise[Unit] = ???

  def save(filename: String, contents: String): js.Promise[Unit] = ???

  def getData(filename: String): js.Promise[String] = ???
}

object LineAccessFileInputOps {
  def create(): LineAccessFileInputOps = new LineAccessFileInputOps {
    override def readLine(filename: String, position: Long): Future[Array[Char]] = Future {
      val file = new BufferedReader(new FileReader(filename))
      try {
        val line = file.lines().skip(position).findFirst()
        if (line.isPresent) line.get().toCharArray
        else throw new EndOfFileException
      }
      finally {
        file.close()
      }
    }

    override def initialise(filename: String): Future[Unit] = Future {
      val file = new File(filename)
      if (file.exists()) Future {}
      else save(filename, "")
    }

    override def save(filename: String, contents: String): Future[Unit] = Future {
      val writer = new BufferedWriter(new FileWriter(filename))
      try {
        writer.write(contents)
      } finally {
        writer.close()
      }
    }

    override def getData(filename: String): Future[String] = ???
  }

  def createJs(implementation: LineAccessFileInputOpsJs): LineAccessFileInputOps = new LineAccessFileInputOps {
    override def readLine(filename: String, position: Long): Future[Array[Char]] =
      implementation.readLine(filename, position.toDouble).toFuture.map(_.toCharArray)

    override def initialise(filename: String): Future[Unit] =
      implementation.initialise(filename).toFuture

    override def save(filename: String, contents: String): Future[Unit] =
      implementation.save(filename, contents).toFuture

    override def getData(filename: String): Future[String] =
      implementation.getData(filename).toFuture
  }
}
