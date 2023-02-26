module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open Symbol
open BusWireUpdateHelpers

open Optics
open Operators

(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)



/// Function to determine if a segment is intersecting a given Symbol in any way.
/// It returns an Orientation option. The intersection can be Horizontal, Vertical or None.
//let segOverSymbol (symbol: Symbol) (i: int) (segment: Segment): Orientation option =
//    let startPos, endPos = getAbsoluteSegmentPos 
//    let orientation = getSegmentOrientation startPos endPos
//    let bBox = getSymbolBoundingBox symbol

//    /// Function to determine if a point is within a Bounding Box.
//    /// It will return True if the point is within the box, False otherwise.
//    let pointInBBox (point: XYPos) (bBox: BoundingBox): bool =
//        let horizontally = point.X > bBox.TopLeft.X && point.X < bBox.TopLeft.X + bBox.W
//        let vertically = point.Y > bBox.TopLeft.Y && point.Y < bBox.TopLeft.Y - bBox.H
    
//        horizontally && vertically

//    /// Function to determine if and how a segment crosses a symbol from end to end.
//    /// This means that the edges of the segment are outside of the Symbol Bounding Box.
//    let crossesBBox (startPos: XYPos) (endPos: XYPos) (bBox: BoundingBox): bool =
//        let horizontally = (startPos.X < bBox.TopLeft.X) && (endPos.X > bBox.TopLeft.X + bBox.W) && (startPos.Y > bBox.TopLeft.Y) && (startPos.Y < bBox.TopLeft.Y + bBox.H)
//        let vertically = (startPos.Y < bBox.TopLeft.Y) && (endPos.Y > bBox.TopLeft.Y + bBox.H) && (startPos.X > bBox.TopLeft.X) && (startPos.X < bBox.TopLeft.X + bBox.W)
    
//        horizontally || vertically

//    match pointInBBox startPos bBox || pointInBBox endPos bBox || crossesBBox startPos endPos bBox with
//        | true -> Some orientation
//        | false -> None 
    

/// Fuction to detect if a wire has a segment that is overlapping with a Symbol.
//let wireOversymbol (model: Model) (wire: Wire): bool =
//    let symbols = model.Symbol.Symbols
//    let removeFstLast list =
//        list |> List.tail |> List.rev |> List.tail |> List.rev
//    let mid_segments = removeFstLast wire.Segments

//    symbols |> Map.fold (fun segments = )




/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): Wire =
    let wire2 = autoroute model wire
    let symbols = model.Symbol.Symbols
    //let removeFstLast list =
    //    list |> List.tail |> List.rev |> List.tail |> List.rev
    //let mid_segments = removeFstLast wire.Segments

    

    let checkWireSegs (Id: ComponentId) (symbol: Symbol) =
        let segFolder (state: bool) (i: int) =
            let startPos, endPos = getAbsoluteSegmentPos wire i
            let bBox = getSymbolBoundingBox symbol

            let intersectAmount = segmentIntersectsBoundingBox bBox startPos endPos

            match intersectAmount with
            | Some num ->
                      //printfn "Segment %A intersects a symbol" i
                      true
            | None ->
                      //printfn "Segment %A does not intersect a Symbol" i
                      state

        (false ,[1..List.length wire.Segments - 1 ]) ||> List.fold segFolder
    
    let test = symbols |> Map.map checkWireSegs
    printfn "%A" test
    if List.exists (fun b -> snd(b)=true) (Map.toList test)
    then {wire2 with Color = Red}
    else wire2
    
    //autoroute model wire
