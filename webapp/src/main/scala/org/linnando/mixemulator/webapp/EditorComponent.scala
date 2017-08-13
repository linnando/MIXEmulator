package org.linnando.mixemulator.webapp

import angulate2.std._

@Component(
  selector = "editor",
  templateUrl = "webapp/src/main/resources/editor.component.html",
  styleUrls = @@@("webapp/src/main/resources/editor.component.css")
)
class EditorComponent(virtualMachineService: VirtualMachineService) {
  var programText: String = virtualMachineService.text

  def changeProgramText(): Unit = virtualMachineService.text = programText
}
