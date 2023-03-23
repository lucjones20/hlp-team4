# Team 4's Readme

## Implemented functionality

### Format Symbols

After selecting 2 symbols, press ctl-F or cmd-Shift-F to format the symbols. This will reorder the ports on both the symbols, then resize the symbol so that all the wires are parallel and the it will clean up the wires in between the two symbols if there are any more wires. If the two symbols are connected in a conflicting way, a popup will give you the choice to either resize from the inner ports or the outer ports. We can also detect whether or not the symbol that is being resized overlaps with any other symbol (currently only prints on the dev console but hopefully we will have a popup by the deadline). 

### Resize Symbols

It is possible to call only resize symbol. The resize symbol also has the conflicting and overlapping mechanism that format symbol has.
* if there are no wires connecting the two symbols then nothing happens
* if there is only one wire connecting the symbols then nothing happens
* works for any orientation
* works with wires that have input and output on the same side of a symbol
* works if the symbols have scales different from 1
* if the wiring is complicated, a popup will ask for clarification for which way the symbols have to be resized
* if the resize will mean that the symbol now overlaps with another symbol it tries to resiize the other symbol, and if that one overlaps too, you will be notified (currently only in the console)

### Smart Channel

Rearange the wires inside a channel


### Smart Port Reordering

Reorder the ports of the first symbol that was selectec.