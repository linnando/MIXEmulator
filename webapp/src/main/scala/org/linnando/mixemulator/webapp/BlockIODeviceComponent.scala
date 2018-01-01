package org.linnando.mixemulator.webapp

import angulate2.std._
import org.linnando.mixemulator.vm.io.data.IOWord
import org.scalajs.dom.raw.{Blob, BlobPropertyBag, FileReader, URL}
import org.scalajs.dom.{File, FileList, UIEvent}
import rxjs.core.Subscription

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scala.util.{Failure, Success}

@Component(
  selector = "block-io-device",
  templateUrl = "webapp/src/main/resources/block-io-device.component.html",
  styleUrls = @@@("webapp/src/main/resources/block-io-device.component.css")
)
class BlockIODeviceComponent(virtualMachineService: VirtualMachineService) extends OnInit with OnDestroy {
  @Input()
  var deviceNum: Int = _
  var stateChangeSubscription: Subscription = _
  var indices: js.Array[Int] = js.Array[Int]()
  var ioWords: IndexedSeq[IOWord] = IndexedSeq.empty
  var inputFile: Option[File] = None

  override def ngOnInit(): Unit = {
    fetchDeviceData()
    stateChangeSubscription = virtualMachineService.stateChange.subscribe(_ => fetchDeviceData())
  }

  private def fetchDeviceData(): Unit = {
    virtualMachineService.blockDeviceData(deviceNum) onComplete {
      case Success(data) =>
        indices = js.Array[Int]() ++ data.indices
        ioWords = data
      case Failure(e) => ErrorPopup.show(e)
    }
  }

  override def ngOnDestroy(): Unit = {
    stateChangeSubscription.unsubscribe()
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
        val buffer = target.result.asInstanceOf[ArrayBuffer]
        val fileData = Array.empty[Byte] ++ new Uint8Array(buffer).map(_.toByte)
        virtualMachineService.saveBlockDevice(deviceNum, fileData) onComplete {
          case Success(_) => fetchDeviceData()
          case Failure(e) => ErrorPopup.show(e)
        }
      }
      reader.readAsArrayBuffer(file)
    case None =>
      throw new Error
  }

  def cellSign(index: Short): String =
    if (ioWords(index).negative) "-" else "+"

  def cellByte(address: Short, pos: Int): Byte =
    ioWords(address).bytes(pos)

  def saveFile(): Unit = {
    val bytes = js.Array[Byte]() ++ ioWords.flatMap(word => {
      val headByte = if (word.negative) (word.bytes.head | 0x80).toByte else word.bytes.head
      headByte :: (1 until 5).map(word.bytes).toList
    }).toArray
    val buffer = new Uint8Array(bytes).buffer
    val blobParts = js.Array[ArrayBuffer](buffer).asInstanceOf[js.Array[js.Any]]
    val options = js.Dynamic.literal("type" -> "application/octet-stream").asInstanceOf[BlobPropertyBag]
    val blob = new Blob(blobParts, options)
    val url = URL.createObjectURL(blob)
    js.Dynamic.global.window.open(url)
  }
}
