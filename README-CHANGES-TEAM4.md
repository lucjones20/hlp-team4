# Team 4's Readme

## Changes to code

src/Renderer/DrawBlock/BusWireUpdate.fs
- added ability to calculate if two bounding boxes intersect each other
- added popup options to the symbols model
- added popup dialogue to the update function
- modified update to also deal with the Selective Resize message

src/Renderer/DrawBlock/PopupDrawingView.fs
- open BusWireT to include BusWireT.Msg instead of SheetT.Msg

src/Renderer/DrawBlock/DrawModelType.fs
- moved popup dialogue to module BusWireT
- changed module BusWireT "type Msg" to module BusWireT "and Msg" to be compatable with PopupDrawingView
- moved Popup Dialog Management Messages into module BusWireT "and Msg"
- added TestFormatSymbol to module SheetT "type Msg"
- added the SelectiveResize message to the BusWireT Msg type

src/Renderer/DrawBlock/SheetUpdate.fs
- all changes are just implementing the group phase changes

src/Renderer/DrawBlock/SheetUpdateHelpers.fs
- includes commented out code that can resize on a click on one symbol if two are highlighted, currently not implemented fully
- corrected the topLeft value to be the right value

src/Renderer/DrawBlock/SmartHelpers.fs
- popups added to bottom of this file as not sure where else to put them

src/Renderer/Renderer.fs
- moved menus around to add a help menu with descriptions of some functionality and a link to the issie website

src/Renderer/UI/MainView.fs
- added popup Drawing View functinoality