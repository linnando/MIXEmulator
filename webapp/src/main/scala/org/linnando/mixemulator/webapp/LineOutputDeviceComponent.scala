package org.linnando.mixemulator.webapp

import angulate2.std._
import org.scalajs.dom.raw.{Blob, BlobPropertyBag, URL}
import rxjs.core.Subscription

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.{Failure, Success}

@Component(
  selector = "line-output-device",
  templateUrl = "webapp/src/main/resources/line-output-device.component.html",
  styleUrls = @@@("webapp/src/main/resources/line-output-device.component.css")
)
class LineOutputDeviceComponent(virtualMachineService: VirtualMachineService) extends OnInit with OnDestroy {
  @Input()
  var deviceNum: Int = _
  var stateChangeSubscription: Subscription = _
  var lines: js.Array[String] = js.Array()

  override def ngOnInit(): Unit = {
    fetchDeviceData()
    stateChangeSubscription = virtualMachineService.stateChange.subscribe(_ => fetchDeviceData())
  }

  private def fetchDeviceData(): Unit = {
    virtualMachineService.lineDeviceData(deviceNum) onComplete {
      case Success(data) => lines = js.Array[String]() ++ data
      case Failure(e) => ErrorPopup.show(e)
    }
  }

  override def ngOnDestroy(): Unit = {
    stateChangeSubscription.unsubscribe()
  }

  def saveFile(): Unit = {
    val blobParts = lines.map(_ + "\n").asInstanceOf[js.Array[js.Any]]
    val options = js.Dynamic.literal("type" -> "text/plain").asInstanceOf[BlobPropertyBag]
    val blob = new Blob(blobParts, options)
    val url = URL.createObjectURL(blob)
    js.Dynamic.global.window.open(url)
  }
}
