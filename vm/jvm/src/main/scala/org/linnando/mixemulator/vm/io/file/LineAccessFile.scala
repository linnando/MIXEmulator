package org.linnando.mixemulator.vm.io.file

import java.io._
import java.util.function.Consumer

import org.linnando.mixemulator.vm.exceptions.EndOfFileException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object LineAccessFile {
  def readLine(filename: String, position: Long): Future[Array[Char]] = Future {
    val file = new BufferedReader(new FileReader(filename))
    try {
      file.skip(position)
      val line = file.readLine()
      if (line == null)
        throw new EndOfFileException
      line.toCharArray
    }
    finally {
      file.close()
    }
  }

  def appendLine(filename: String, version: Int, chars: Array[Char]): Future[Unit] = Future {
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

  def appendNewPage(filename: String, version: Int): Future[Unit] = Future {
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

  def initialiseNonVersioned(filename: String): Future[Unit] = {
    val file = new File(filename)
    if (file.exists()) Future {}
    else saveNonVersioned(filename, "")
  }

  def saveNonVersioned(filename: String, contents: String): Future[Unit] = Future {
    val writer = new BufferedWriter(new FileWriter(filename))
    try {
      writer.write(contents)
    } finally {
      writer.close()
    }
  }

  def initialiseVersioned(filename: String): Future[Unit] = Future {
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

  def getData(filename: String): Future[String] = ???

  def getData(filename: String, version: Int): Future[String] = ???
}
