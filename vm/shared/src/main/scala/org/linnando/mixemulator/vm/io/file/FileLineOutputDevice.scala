package org.linnando.mixemulator.vm.io.file

import java.io._
import java.util.function.Consumer

import org.linnando.mixemulator.vm.io.PositionalOutputDevice
import org.linnando.mixemulator.vm.io.data.IOWord

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success

trait FileLineOutputDevice extends PositionalOutputDevice {
  def filename: String
  def version: Int
  def tasks: Future[Unit]

  def write(words: IndexedSeq[IOWord]): FileLineOutputDevice =
    newVersion(tasks andThen { case Success(_) => writeLine(words) })

  def newVersion(tasks: Future[Unit]): FileLineOutputDevice

  def writeLine(words: IndexedSeq[IOWord]): Unit = {
    val chars = new Array[Char](5 * blockSize)
    val buffer = mutable.ArrayBuffer.empty[Char]
    buffer ++= words.flatMap(_.toChars)
    buffer.copyToArray(chars)
    val oldFile = new File(s"$filename.$version")
    val writer = new BufferedWriter(new FileWriter(s"$filename.${version + 1}"))
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

  override def flush(): (FileLineOutputDevice, Seq[IndexedSeq[IOWord]]) =
    (withoutTasks, { Await.ready(tasks, 1 seconds); Seq.empty })

  def withoutTasks: FileLineOutputDevice
}
