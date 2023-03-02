## Contributions

Luc Jones lxj19 (AUTHOR Jones): 
- [SmartSizeSymbol](src/Renderer/DrawBlock/SmartSizeSymbol.fs) -> all functions in file
- [SmartHelpers](src/Renderer/DrawBlock/SmartHelpers.fs) -> some helper functions
- [SheetUpdateHelpers](src/Renderer/DrawBlock/SheetUpdateHelpers.fs) -> added a call to reSizeSymbol in the InitialiseMoving case (line 520) so that symbols can be resized simply (more intuitive than a gui symbol in my opinion, hence why I added this (can be easily changed if you think it is not intuitive / if there is a better alternative))
- [SheetUpdate](src/Renderer/DrawBlock/SheetUpdate.fs) -> had to pass the updateSymbolWires function to be able to re-draw the wires once the changes to the symbol size had been made (I actually had to make updateSymbolWiresNotSmart which is the same as updateSymbolWires but instead of using smartAutoroute, it uses autoroute; this was because smartAutoroute caused bugs when updating wires)

Konstantinos Sougioultzolgou ks2220 (AUTHOR Sougioultzoglou): 
- [SmartChannel](src/Renderer/DrawBlock/SmartChannel.fs) -> all functions in file
- [SmartHelpers](src/Renderer/DrawBlock/SmartHelpers.fs) -> some helper functions
- [SheetUpdate](src/Renderer/DrawBlock/SheetUpdate.fs) -> Added functionaloty for both vertical and horizontal channel testing 

Angus Parry:
- [SmartPortOrder](src/Renderer/DrawBlock/SmartPortOrder.fs)

Theodoros Gkamaletsos:
- [SmartAutoroute](src/Renderer/DrawBlock/SmartWire.fs)