import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { CollapseModule } from 'ngx-bootstrap/collapse';
import { ModalModule } from 'ngx-bootstrap/modal';
import { AboutComponent } from './about/about.component';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { BlockIoDeviceComponent } from './block-io-device/block-io-device.component';
import { DevicesComponent } from './devices/devices.component';
import { EditorComponent } from './editor/editor.component';
import { ErrorPopupComponent } from './error-popup/error-popup.component';
import { LineInputDeviceComponent } from './line-input-device/line-input-device.component';
import { LineOutputDeviceComponent } from './line-output-device/line-output-device.component';
import { MemoryDataComponent } from './memory-data/memory-data.component';
import { MemoryTextComponent } from './memory-text/memory-text.component';
import { RegistersComponent } from './registers/registers.component';
import { VirtualMachineComponent } from './virtual-machine/virtual-machine.component';

@NgModule({
  declarations: [
    AppComponent,
    AboutComponent,
    BlockIoDeviceComponent,
    DevicesComponent,
    EditorComponent,
    LineInputDeviceComponent,
    LineOutputDeviceComponent,
    MemoryDataComponent,
    MemoryTextComponent,
    RegistersComponent,
    VirtualMachineComponent,
    ErrorPopupComponent
  ],
  imports: [
    AppRoutingModule,
    BrowserAnimationsModule,
    BrowserModule,
    CollapseModule.forRoot(),
    FontAwesomeModule,
    FormsModule,
    ModalModule.forRoot(),
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
