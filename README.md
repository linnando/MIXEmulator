# MIX Emulator

MIX Emulator is a web-based emulator of [MIX](https://en.wikipedia.org/wiki/MIX), the mythical computer
invented by Donald Knuth for his book series
[&quot;The Art of Computer Programming&quot;](http://www-cs-faculty.stanford.edu/~knuth/taocp.html). The emulator
supports assembling and executing programs for MIX, inspecting memory and register contents, working with (virtual)
peripheral devices, and traversing execution flow in forward and backward direction. Programs may be executed in either
the binary or the decimal mode (which differ in the byte size). Working with devices includes checking correct
synchronisation of input/output operations: the emulator fails when the result of an operation is undetermined (e.g., on
an attempt to write data to the memory block that is sent to a device without ensuring that the output operation has
completed).

The terminal device and floating-point operations are not supported currently.

A working copy of the emulator is available at [http://www.mix-emulator.org](http://www.mix-emulator.org).

If the emulator is useful in your research, consider please referencing the following paper:

> Batdalov, R., Ņikiforova, O. Implementation of a MIX Emulator: A Case Study of the Scala Programming Language
> Facilities. *Applied Computer Systems*, 2017, 22, pp.47-53. ISSN 2255-8683. e-ISSN 2255-8691. Available from:
> [doi:10.1515/acss-2017-0017](http://dx.doi.org/10.1515/acss-2017-0017)

## Acknowledgment

MIX Emulator is powered by [Scala](http://scala-lang.org), [Scala.js](http://www.scala-js.org/),
[Angular](https://angular.io/), [Bootstrap](http://getbootstrap.com/), 
[ngx-bootstrap](https://valor-software.com/ngx-bootstrap/#/), [Ace](https://ace.c9.io/), 
[Font Awesome](http://fontawesome.io/), and [Specs2](https://etorreborre.github.io/specs2/). The author also
thanks [JetBrains](https://www.jetbrains.com/) for the provided educational IntelliJ IDEA licence.
