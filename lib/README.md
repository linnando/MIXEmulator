# MIX Emulator Library

The backend part of [MIX Emulator](http://www.mix-emulator.org), a web-based emulator of
[MIX](https://en.wikipedia.org/wiki/MIX), the mythical computer invented by Donald Knuth for his
book series [&quot;The Art of Computer
Programming&quot;](http://www-cs-faculty.stanford.edu/~knuth/taocp.html). The library supports
assembling and executing programs for MIX, inspecting memory and register contents, working with
(virtual) peripheral devices, and traversing execution flow in forward and backward direction.
Programs may be executed in either the binary or the decimal mode (which differ in the byte size).
Working with devices includes checking correct synchronisation of input/output operations: the
emulator fails when the result of an operation is undetermined (e.g., on an attempt to write data
to the memory block that is sent to a device without ensuring that the output operation has
completed).

The terminal device and floating-point operations are not supported currently.

It is a cross-platform library, written in [Scala](http://scala-lang.org) and transpiled into
JavaScript with [Scala.js](http://www.scala-js.org/). The JavaScript code is accompanied by `.d.ts`
type definitions.

## Usage

1. Subclass `BlockAccessFileOps`, `LineAccessFileInputOps`, and `LineAccessFileOutputOps` and
implement the declared input/output operations using the storage you like.

1. Pass instances of the input/output implementation classes to `DeviceFrontEnd$.create` and get an
instance of `DeviceFrontEnd`.

1. Use one of the methods defined in `VirtualMachineFrontEnd$` to create an instance of
`VirtualMachineFrontEnd`.

1. Use the instance of `VirtualMachineFrontEnd` to execute the program and inspect the virtual
machine state.

## Reference

If the emulator is useful in your research, consider please referencing the following paper:

> Batdalov, R., Ņikiforova, O. Implementation of a MIX Emulator: A Case Study of the Scala Programming Language
> Facilities. *Applied Computer Systems*, 2017, 22, pp.47-53. ISSN 2255-8683. e-ISSN 2255-8691. Available from:
> [doi:10.1515/acss-2017-0017](http://dx.doi.org/10.1515/acss-2017-0017)
