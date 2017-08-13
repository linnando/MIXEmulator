package org.linnando.mixemulator.webapp

import angulate2.router.Router
import angulate2.std._

@Component(
  selector = "mix-emulator-app",
  templateUrl = "webapp/src/main/resources/app.component.html",
  styleUrls = @@@("webapp/src/main/resources/app.component.css")
)
class AppComponent(r: Router) {
  val router: Router = r
}
