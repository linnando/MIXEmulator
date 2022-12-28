import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AboutComponent } from './about/about.component';
import { DevicesComponent } from './devices/devices.component';
import { EditorComponent } from './editor/editor.component';
import { VirtualMachineComponent } from './virtual-machine/virtual-machine.component';

const routes: Routes = [
  { path: '', redirectTo: '/about', pathMatch: 'full' },
  { path: 'about', component: AboutComponent },
  { path: 'devices', component: DevicesComponent },
  { path: 'editor', component: EditorComponent },
  { path: 'vm', component: VirtualMachineComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes, { useHash: true })],
  exports: [RouterModule]
})
export class AppRoutingModule { }
