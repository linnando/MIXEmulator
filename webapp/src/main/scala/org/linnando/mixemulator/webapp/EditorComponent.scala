package org.linnando.mixemulator.webapp

import angulate2.core.AfterViewInit
import angulate2.std._
import com.scalawarrior.scalajs.ace._

import scala.scalajs.js

@Component(
  selector = "editor",
  templateUrl = "webapp/src/main/resources/editor.component.html",
  styleUrls = @@@("webapp/src/main/resources/editor.component.css")
)
class EditorComponent(virtualMachineService: VirtualMachineService) extends AfterViewInit {
  override def ngAfterViewInit(): Unit = {
    val editor = ace.edit("editor")
    editor.setTheme("ace/theme/chrome")
    editor.setValue(virtualMachineService.text)
    editor.getSession().setMode("ace/mode/mixal")
    editor.getSession().setUseSoftTabs(true)
    editor.getSession().on("change", (_: js.Any) => virtualMachineService.text = editor.getValue())
  }
}
