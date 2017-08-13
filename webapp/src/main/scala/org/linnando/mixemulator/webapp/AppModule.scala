package org.linnando.mixemulator.webapp

import angulate2.forms.FormsModule
import angulate2.std._
import angulate2.platformBrowser.BrowserModule
import angulate2.router.RouterModule

import scala.scalajs.js

@NgModule(
  imports = @@[BrowserModule, FormsModule] :+
    RouterModule.forRoot(@@@(
      Route(path = "", redirectTo = "/about", pathMatch = "full"),
      Route(path = "about", component = %%[AboutComponent]),
      Route(path = "editor", component = %%[EditorComponent]),
      Route(path = "vm", component = %%[VirtualMachineComponent])
    ), js.Dynamic.literal(useHash = true)),
  declarations = @@[
    AboutComponent,
    AppComponent,
    EditorComponent,
    MemoryDataComponent,
    MemoryTextComponent,
    RegistersComponent,
    VirtualMachineComponent
  ],
  providers = @@[VirtualMachineService],
  bootstrap = @@[AppComponent]
)
class AppModule {}
