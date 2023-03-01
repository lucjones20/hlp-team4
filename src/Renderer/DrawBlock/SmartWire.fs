module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open Symbol
open BusWireUpdateHelpers
open SmartHelpers

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



    

/// Fuction to detect if a wire has a segment that is overlapping with a Symbol.
//let wireOversymbol (model: Model) (wire: Wire): bool =
//    let symbols = model.Symbol.Symbols
//    let removeFstLast list =
//        list |> List.tail |> List.rev |> List.tail |> List.rev
//    let mid_segments = removeFstLast wire.Segments

//    symbols |> Map.fold (fun segments = )


/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
//let smartAutoroute (model: Model) (wire: Wire): Wire =
//    printfn "-----------------New Autoroute-------------------"
//    let wire2 = autoroute model wire
//    let symbols = model.Symbol.Symbols
//    //let removeFstLast list =
//    //    list |> List.tail |> List.rev |> List.tail |> List.rev
//    //let mid_segments = removeFstLast wire.Segments
    

//    let checkWireSegs (Id: ComponentId) (symbol: Symbol) =
//        printfn "Symbol %A" symbol.Id
//        let segFolder (state: bool) (i: int) =
//            let startPos, endPos = getAbsoluteSegmentPos wire i
//            let bBox = getSymbolBoundingBox symbol

//            let intersectAmount = segmentIntersectsBoundingBox bBox startPos endPos

//            let intersect = segOverSymbol symbol i wire2
//            printfn "Intersection %A segment %A" intersect i

//            match intersect with
//            | Some orientation ->
//                      //printfn "Segment %A intersects a symbol" i
//                      true
//            | None ->
//                      //printfn "Segment %A does not intersect a Symbol" i
//                      state

//        (false ,[1..List.length wire.Segments - 1 ]) ||> List.fold segFolder
    
//    let test = symbols |> Map.map checkWireSegs
//    printfn "%A" test
//    printfn "Found intesecting segment %A" (List.exists (fun b -> snd(b)=true) (Map.toList test))
//    if List.exists (fun b -> snd(b)=true) (Map.toList test)
//    then {wire2 with Color = Red}
//    else wire2
    
    //autoroute model wire




let smartAutoroute (model: Model) (wire: Wire): Wire =
    printfn "-----------------New Autoroute-------------------"
    let wire2 = autoroute model wire
    let newModel = updateWireSegmentJumps [wire2.WId] (Optic.set (wireOf_ wire2.WId) wire2 model)
    printfn "Segments: %A" wire2.Segments
    printfn "Wire has %A segments" (List.length wire2.Segments)
    let symbols = model.Symbol.Symbols
    //let removeFstLast list =
    //    list |> List.tail |> List.rev |> List.tail |> List.rev
    //let mid_segments = removeFstLast wire.Segments
    

    let checkWireSegs (wire3: Wire) (el: ComponentId*Symbol) =
        let symbol = snd(el)
        printfn "Symbol %A" symbol.Id
        let segFolder (wire4: Wire) (i: int) =
            if (i <= (List.length wire4.Segments) - 1) && (wire4.Segments.[i].Length <> 8.0)
            then
                let startPos, endPos = getAbsoluteSegmentPos wire4 i
                let bBox = getSymbolBoundingBox symbol

                //let intersectAmount = segmentIntersectsBoundingBox bBox startPos endPos

            

                let intersect = segOverSymbol symbol i wire4
                //printfn "Intersection %A segment %A" intersect i
                //printfn "startPos is %A" startPos

                match intersect with
                | Some orientation ->
                          printfn "Intersection %A segment %A" intersect i
                          match orientation with
                          | Horizontal ->
                                if startPos.Y < (bBox.TopLeft.Y + (bBox.H / 2.0))
                                then
                                        let intDistance = -((startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0))) + (bBox.H / 2.0) + 10.0)
                                        moveSegment newModel wire4.Segments.[i] intDistance
                                else
                                        let intDistance = (bBox.H / 2.0) + 10.0 - (startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0)))
                                        //printfn "distance moved %A - %A = %A" startPos.Y (bBox.TopLeft.Y + (bBox.H / 2.0)) intDistance
                                        moveSegment newModel wire4.Segments.[i] intDistance

                          | Vertical ->
                                if startPos.X < (bBox.TopLeft.X + (bBox.W / 2.0))
                                then
                                    let intDistance = -((bBox.W / 2.0) + 10.0 + (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))))
                                    printfn "distance moved %A" intDistance
                                    moveSegment newModel wire4.Segments.[i] intDistance
                                else
                                    let intDistance = (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))) - (bBox.W / 2.0) + 10.0
                                    //printfn "distance moved %A" intDistance
                                    moveSegment newModel wire4.Segments.[i] intDistance

                | None ->
                          //printfn "Not moved"
                          wire4
            else
                wire4

        (wire3 ,[0..10]) ||> List.fold segFolder
    
    (wire2, Map.toList symbols) ||> List.fold checkWireSegs

    //autoroute model wire