# Team 4's Readme

## Implemented functionality

### Format Symbols

After selecting 2 symbols, press Ctrl-Shift-F or Cmd-Shift-F to format the symbols. This will reorder the ports on both the symbols, then resize the symbol so that all the wires are parallel and the it will clean up the wires in between the two symbols if there are any more wires. If the two symbols are connected in a conflicting way, a popup will give you the choice to either resize from the inner ports or the outer ports. We can also detect whether or not the symbol that is being resized overlaps with any other symbol. A popup was made that ask the user what ports the resize should be based on, or if he would like the resize not to occur at all. 

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
* Works for all orientations
* menu item for calling smart channel separately
* works well alongside smart port reodreding and smart resizing


### Smart Port Reordering

Reorder the ports of the first symbol that was selected. 
* menu item for calling port reordering separately
* works very well with all ports that are opposite each other
* works well with ports that are on the far side of symbols opposite each other
* doesn't sort ports that are connected to different edges of symbols to each other (i.e won't sort a port right-top with a port right-left)
* works on all symbols that can connect multiple ports to another individual symbol
