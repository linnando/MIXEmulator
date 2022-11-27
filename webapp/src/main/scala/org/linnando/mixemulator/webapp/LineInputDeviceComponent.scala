package org.linnando.mixemulator.webapp

import angulate2.router.Router
import angulate2.std._
import org.scalajs.dom
import org.scalajs.dom.raw.{Blob, BlobPropertyBag, FileReader, URL}
import org.scalajs.dom.{FileList, UIEvent, html}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.{Failure, Success}

@Component(
  selector = "line-input-device",
  templateUrl = "webapp/src/main/resources/line-input-device.component.html",
  styleUrls = @@@("webapp/src/main/resources/line-input-device.component.css")
)
class LineInputDeviceComponent(router: Router, virtualMachineService: VirtualMachineService) extends OnInit {
  @Input() var deviceNum: Int = _
  @Input() var hasGoButton = false
  var lines: js.Array[String] = js.Array()

  override def ngOnInit(): Unit = {
    fetchDeviceData()
  }

  private def fetchDeviceData(): Unit = virtualMachineService.lineDeviceData(deviceNum) onComplete {
    case Success(data) => lines = js.Array[String]() ++ data
    case Failure(e) => ErrorPopup.show(e)
  }

  def canUploadFile: Boolean = !virtualMachineService.isActive

  def startFileUpload(): Unit = {
    dom.document.getElementById("inputFile").asInstanceOf[html.Input].click()
  }

  def uploadFile(files: FileList): Unit =
    if (files.length > 0) {
      val file = files(0)
      val reader = new FileReader()
      reader.onload = (event: UIEvent) => {
        val target = event.target.asInstanceOf[js.Dynamic]
        val fileData = target.result.asInstanceOf[String]
        VirtualMachineService.saveLineDevice(deviceNum, fileData) onComplete {
          case Success(_) => fetchDeviceData()
          case Failure(e) => ErrorPopup.show(e)
        }
      }
      reader.readAsText(file)
    }

  def downloadFile(): Unit = {
    val blobParts = lines.map(_ + "\n").asInstanceOf[js.Array[js.Any]]
    val options = js.Dynamic.literal("type" -> "text/plain").asInstanceOf[BlobPropertyBag]
    val blob = new Blob(blobParts, options)
    val url = URL.createObjectURL(blob)
    js.Dynamic.global.window.open(url)
  }

  def mode: String = virtualMachineService.mode

  def mode_=(value: String): Unit = virtualMachineService.mode = value

  def tracking: Boolean = virtualMachineService.tracking

  def tracking_=(value: Boolean): Unit = virtualMachineService.tracking = value

  def go(): Unit = virtualMachineService.go() onComplete {
    case Success(_) => router.navigate(js.Array("/vm"))
    case Failure(e) => ErrorPopup.show(e)
  }
}
