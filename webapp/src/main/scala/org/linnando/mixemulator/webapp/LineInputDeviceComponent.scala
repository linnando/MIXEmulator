package org.linnando.mixemulator.webapp

import angulate2.std._
import org.scalajs.dom.raw.{Blob, BlobPropertyBag, FileReader, URL}
import org.scalajs.dom.{File, FileList, UIEvent}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.{Failure, Success}

@Component(
  selector = "line-input-device",
  templateUrl = "webapp/src/main/resources/line-input-device.component.html",
  styleUrls = @@@("webapp/src/main/resources/line-input-device.component.css")
)
class LineInputDeviceComponent(virtualMachineService: VirtualMachineService) extends OnInit {
  @Input()
  var deviceNum: Int = _
  var lines: js.Array[String] = js.Array()
  var inputFile: Option[File] = None

  override def ngOnInit(): Unit = {
    fetchDeviceData()
  }

  private def fetchDeviceData(): Unit = {
    virtualMachineService.lineDeviceData(deviceNum) onComplete {
      case Success(data) => lines = js.Array[String]() ++ data
      case Failure(e) => ErrorPopup.show(e)
    }
  }

  def canUploadFile: Boolean = !virtualMachineService.isActive

  def onFileChange(files: FileList): Unit = {
    if (files.length > 0) inputFile = Some(files(0))
    else inputFile = None
  }

  def fileIsNotChosen: Boolean = inputFile.isEmpty

  def loadFile(): Unit = inputFile match {
    case Some(file) =>
      val reader = new FileReader()
      reader.onload = (event: UIEvent) => {
        val target = event.target.asInstanceOf[js.Dynamic]
        val fileData = target.result.asInstanceOf[String]
        virtualMachineService.saveLineDevice(deviceNum, fileData) onComplete {
          case Success(_) => fetchDeviceData()
          case Failure(e) => ErrorPopup.show(e)
        }
      }
      reader.readAsText(file)
    case None =>
      throw new Error
  }

  def saveFile(): Unit = {
    val blobParts = lines.map(_ + "\n").asInstanceOf[js.Array[js.Any]]
    val options = js.Dynamic.literal("type" -> "text/plain").asInstanceOf[BlobPropertyBag]
    val blob = new Blob(blobParts, options)
    val url = URL.createObjectURL(blob)
    js.Dynamic.global.window.open(url)
  }
}
