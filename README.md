STATUS: still debugging baseline.  Failing in (test6) on some mismatch in OOP pattern-matching.
	I made a sweeping change, creating input-pins and output-pins and have not finished debugging
	the repercussions yet.

USAGE: 
lisp> (ql:quickload :cl-event-passing)
lisp> (cl-event-passing::test)


ESSAY:

Event-passing system for CL.

Uses concurrent components (Parts) based on mutual multitasking instead of O/S threads.

Components are hierarchical. Parts can be Schematics or Leaf-nodes.

Schematics contain other Part instances (its children) plus a wiring list of how the children are connected (piped) together.

I've tried to be very explicit about every kind of object, which results in lots of syntactic noise.  See the diagram in Parts.drawio (NB it contains two or more tabs).  Many parts of this could be optimized better, but are not optimized, for clarity.

One should be able to use these ideas to build pipelines (a la /bin/*sh).  Hierarchical pipelines allowing for feedback.

The Dispatcher runs the show, selecting "ready" parts "at random"xs

A selected "ready" Part process one event to completion, then "return" to the Dispatcher.

Each Part has an input and an output queue.

A Part cannot consume another input event until it has finished processing the current event.  This means that a Schematic is "busy" if any of its children are busy.

An event is a two-tuple.  An input event is {input-pin,data} and is placed onto the input queue of the receiving Part.  An output event is {output-pin,data} and is placed onto the output queue of the sending Part.

Parts cannot refer to other Parts (e.g. cannot refer to their peers).  Parts can only SEND events.  SENDing an event puts the event onto the output queue of the Part until the Part is finished processing one event.  The Output Queue is released by the Dispatcher and all output events are converted to input events according to the wiring list.

Namespaces - a Part is defined by its "kind" (basically its Class) plus the names of its input pins plus the names of its output pins.  Input pin and output pin namespaces are distinct (and can contain the same names).

APIs - a Wire can eminate from an output pin to zero or more destination Pins.  Typically, destination pins are input pins of children Parts of a Schematic. A destination pin might also be the "inside" of an output pin of a Schematic Part. Also, a wire can eminate from the "inside" of an input pin of a Schematic to any destination(s).

A Leaf Part consists of "code" that is executed (like a callback) once for every event on its input queue (determined by the Dispatcher).

Referential Transparency - a Part can be replaced by any Part that has the same pin-out.  It cannot be known whether a Part is implemented as a hierarchical Schematic or as a Leaf.

Time Ordering - an Event sent to a Wire arrives at the destinations "at the same time", i.e. Event delivery is atomic (this matters in a multi-threaded environment, such as hardware with interrupts (i.e. interrupts are masked OFF during delivery of a single event to all destinations on a Wire)).  Time Ordering in a truly distributed environment is currently Undefined and is the resposibility of the Architect (i.e. latency matters in a distributed environment ; methods for handling such cases are described in hardware literature).

Diagrams: cl-event-passing programs can be diagrammed much in the same way as found in Electronics CAD systems.  The feature of full concurrency makes it possible to a network diagram of Parts.

Rules of Thumb:
1. Keep schematics (and diagrams) down to about 7 plus/minus 2 Parts.
2. Every schematic must make "sense" on its own.  Children cannot change the behavior of parent Schematics.  Composition, not inheritance, is used.
3. Top-down and bottom-up design are, both, OK.
4. Schematics (esp. diagrams of schematics) can be used to discuss and hone the Architecure of a system "in an Agile manner".
5. Use referential transparency to build a system that works, then "optimize" it later (by replacing pin-compatible Parts).
6. The combination of Dispatcher-based concurrency and output queues results in minimum dependencies between Parts.
7. Testing: each Part can be tested on its own, e.g. using a Truth Table as is done with digital hardware ICs.  More elaborate testing processes, e.g. back-to-back, signature tracing, can be used (explore existing literature / tools for hardware testing).
8. Each Part has an INPUT API *and* an OUTPUT API.
9. Data is not moved throughout the system using parameters.  SEND is the only data (event) passing mechansim.
10.  Leaf parts can resort to any language and may use CALL-RETURN protocols internally (along with parameter passing). The selection of leaf-languages is determined by the implementation.  For example, a JS-only implementation is possible (and, e.g. uses JSON to describe Schematic graphs).  For example, any language supported by Linux, is possible if the implemenation of Schematics uses Linux processes (commands), or, if the LOADer employs ELF.

Hints/Comments:
- Wires hold receivers (pairs: {receiver-part, receiver-pin}).  Receivers are like Listeners, except: (1) that a "wire" "declares" who is listening to what, and (2) a "wire" shows (graphically?) what the Architect intended, there is no dynamic re-wiring, all Listeners are declared at the beginning of time and the network is not dynamically reconfigured(the Architect can provide dynamic reconfiguration Parts, if the solution requires them)
