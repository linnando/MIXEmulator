package org.linnando.mixemulator.vm.io.file

import java.io._
import java.util.function.Consumer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

trait LineAccessFileOutputOps {
  def appendLine(filename: String, version: Int, chars: Array[Char]): Future[Unit]

  def appendNewPage(filename: String, version: Int): Future[Unit]

  def initialise(filename: String): Future[Unit]

  def getVersions(filename: String): Future[Iterable[String]]

  def getCurrentData(filename: String): Future[String] = {
    val contents = for {
      versions: Iterable[String] <- getVersions(filename)
      versionNumbers = versions.map(_.toInt)
      contents: String <- getData(filename, versionNumbers.max)
    } yield contents
    contents
  }

  def getData(filename: String, version: Int): Future[String]
}

@JSExportTopLevel("LineAccessFileOutputOps")
class LineAccessFileOutputOpsJs extends js.Object {
  def appendLine(filename: String, version: Int, chars: String): js.Promise[Unit] = ???

  def appendNewPage(filename: String, version: Int): js.Promise[Unit] = ???

  def initialise(filename: String): js.Promise[Unit] = ???

  def getVersions(filename: String): js.Promise[js.Array[String]] = ???

  def getData(filename: String, version: Int): js.Promise[String] = ???
}

object LineAccessFileOutputOps {
  def create(): LineAccessFileOutputOps = new LineAccessFileOutputOps {
    override def appendLine(filename: String, version: Int, chars: Array[Char]): Future[Unit] = Future {
      val oldFile = new File(s"$filename/$version")
      val writer = new BufferedWriter(new FileWriter(s"$filename/${version + 1}"))
      try {
        if (oldFile.exists())
          copyFileContent(oldFile, writer)
        writer.write(chars)
        writer.newLine()
      }
      finally {
        writer.close()
      }
    }

    protected def copyFileContent(source: File, destWriter: BufferedWriter): Unit = {
      val srcReader = new BufferedReader(new FileReader(source))
      srcReader.lines().forEach(new Consumer[String] {
        override def accept(s: String): Unit = {
          destWriter.write(s)
          destWriter.newLine()
        }
      })
      srcReader.close()
    }

    override def appendNewPage(filename: String, version: Int): Future[Unit] = Future {
      val oldFile = new File(s"$filename/$version")
      val writer = new BufferedWriter(new FileWriter(s"$filename/${version + 1}"))
      try {
        if (oldFile.exists)
          copyFileContent(oldFile, writer)
        writer.write("\f")
      }
      finally {
        writer.close()
      }
    }

    override def initialise(filename: String): Future[Unit] = Future {
      val directory = new File(filename)
      directory.mkdirs()
      val versions = directory.listFiles
      val writer = new BufferedWriter(new FileWriter(s"$filename/0"))
      try {
        writer.write("")
        versions.filter(_.getName != "0").foreach(_.delete())
      } finally {
        writer.close()
      }
    }

    override def getVersions(filename: String): Future[Iterable[String]] = Future {
      val directory = new File(filename)
      directory.mkdirs()
      directory.listFiles().map(_.getName)
    }

    override def getData(filename: String, version: Int): Future[String] = ???
  }

  def createJs(implementation: LineAccessFileOutputOpsJs): LineAccessFileOutputOps = new LineAccessFileOutputOps {
    override def appendLine(filename: String, version: Int, chars: Array[Char]): Future[Unit] =
      implementation.appendLine(filename, version, new String(chars)).toFuture

    override def appendNewPage(filename: String, version: Int): Future[Unit] =
      implementation.appendNewPage(filename, version).toFuture

    override def initialise(filename: String): Future[Unit] =
      implementation.initialise(filename).toFuture

    override def getVersions(filename: String): Future[Iterable[String]] =
      implementation.getVersions(filename).toFuture.map(_.toIterable)

    override def getData(filename: String, version: Int): Future[String] =
      implementation.getData(filename, version).toFuture
  }
}
