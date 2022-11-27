package org.linnando.mixemulator.webapp

import angulate2.core.AfterViewInit
import angulate2.router.Router
import angulate2.std._
import com.scalawarrior.scalajs.ace._
import org.scalajs.dom
import org.scalajs.dom.raw.{Blob, BlobPropertyBag, FileReader, URL}
import org.scalajs.dom.{File, FileList, UIEvent, html}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.{Failure, Success}

@Component(
  selector = "editor",
  templateUrl = "webapp/src/main/resources/editor.component.html",
  styleUrls = @@@("webapp/src/main/resources/editor.component.css")
)
class EditorComponent(router: Router, virtualMachineService: VirtualMachineService) extends AfterViewInit {
  var editor: Editor = _

  override def ngAfterViewInit(): Unit = {
    editor = ace.edit("editor")
    editor.setTheme("ace/theme/chrome")
    editor.setValue(virtualMachineService.text)
    editor.getSession().setMode("ace/mode/mixal")
    editor.getSession().setUseSoftTabs(true)
    editor.getSession().on("change", (_: js.Any) => virtualMachineService.text = editor.getValue())
  }

  def startFileUpload(): Unit = {
    dom.document.getElementById("inputFile").asInstanceOf[html.Input].click()
  }

  def uploadFile(files: FileList): Unit =
    if (files.length > 0) {
      val file = files(0)
      val reader = new FileReader()
      reader.onload = (event: UIEvent) => {
        val target = event.target.asInstanceOf[js.Dynamic]
        val programText = target.result.asInstanceOf[String].replaceAll("\t", "    ")
        editor.setValue(programText)
      }
      reader.readAsText(file)
    }

  def downloadFile(): Unit = {
    val blobParts = js.Array[String](virtualMachineService.text).asInstanceOf[js.Array[js.Any]]
    val options = js.Dynamic.literal("type" -> "text/plain").asInstanceOf[BlobPropertyBag]
    val blob = new Blob(blobParts, options)
    val url = URL.createObjectURL(blob)
    js.Dynamic.global.window.open(url)
  }

  def mode: String = virtualMachineService.mode

  def mode_=(value: String): Unit = virtualMachineService.mode = value

  def tracking: Boolean = virtualMachineService.tracking

  def tracking_=(value: Boolean): Unit = virtualMachineService.tracking = value

  def assemble(): Unit = virtualMachineService.assemble() onComplete {
    case Success(_) => router.navigate(js.Array("/vm"))
    case Failure(e) => ErrorPopup.show(e)
  }
}
