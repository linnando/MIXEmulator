package org.linnando.mixemulator.webapp

import angulate2.std._

@Component(
  selector = "mix-emulator-app",
  template = "<h1>{{greeting}}</h1>"
)
class AppComponent {
  val greeting = "Hello Angular!"
}
