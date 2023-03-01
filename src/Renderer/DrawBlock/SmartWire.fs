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



/// Function to change all segments in a wire to automatic routing mode.
let makeSegsMovable (wire: Wire) :Wire =
    {
        wire with Segments =
        wire.Segments |> List.map (fun seg -> {seg with Mode = Auto})
    }


let makeSegImmov (wire: Wire) (i: int): Wire =
    let newSegs = wire.Segments |> List.map ( fun seg ->
        if seg.Index = i
        then {seg with Mode = Manual}
        else seg )
    {wire with Segments = newSegs}


let removeFstLast list =
    list |> List.tail |> List.rev |> List.tail |> List.rev


let updateWire (model : Model) (wire : Wire) (reverse : bool) =
    let newPort = 
        match reverse with
        | true -> Symbol.getInputPortLocation None model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation None model.Symbol wire.OutputPort
    if reverse then
        partialAutoroute model (reverseWire wire) newPort true
        |> Option.map reverseWire
    else 
        partialAutoroute model wire newPort false
    |> Option.defaultValue wire


//let smartAutoroute (model: Model) (wire: Wire): Wire =
//    printfn "-----------------New Autoroute-------------------"
//    let wireExists = (List.tryFind (fun el -> snd(el).WId = wire.WId) (Map.toList model.Wires)) <> None
//    let wire2 =     match wireExists with
//                    | true  -> wire
//                    | false -> printfn "Wire does not exist"
//                               autoroute model wire
//    let newModel =  match wireExists with
//                    | true  -> model
//                    | false -> updateWireSegmentJumps [wire2.WId] (Optic.set (wireOf_ wire2.WId) wire2 model)
        
//    //printfn "Segments: %A" wire2.Segments
//    //printfn "Wire has %A segments" (List.length wire2.Segments)
//    let symbols = model.Symbol.Symbols
    

//    let checkWireSegs (wire3: Wire) (el: ComponentId*Symbol) =
//        let symbol = snd(el)
//        //printfn "Symbol %A" symbol.Id
//        let segFolder (wire4: Wire) (i: int) =
//            if (i <= (List.length wire4.Segments) - 1) && (wire4.Segments.[i].Length <> 8.0)
//            then
//                let startPos, endPos = getAbsoluteSegmentPos wire4 i
//                let bBox = getSymbolBoundingBox symbol

//                let intersect = segOverSymbol symbol i wire4
//                //printfn "Intersection %A segment %A" intersect i
//                //printfn "startPos is %A" startPos

//                match intersect with
//                | Some orientation ->
//                          printfn "Intersection %A segment %A" intersect i
//                          //let wire5 = makeSegsMovable wire4
//                          match orientation with
//                          | Horizontal ->
//                                if startPos.Y < (bBox.TopLeft.Y + (bBox.H / 2.0))
//                                then
//                                    let intDistance = -((startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0))) + (bBox.H / 2.0) + 10.0)
//                                    printfn "distance moved %A - %A = %A" startPos.Y (bBox.TopLeft.Y + (bBox.H / 2.0)) intDistance
//                                    makeSegImmov (moveSegment newModel wire4.Segments.[i] intDistance) i
//                                else
//                                    let intDistance = (bBox.H / 2.0) + 10.0 - (startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0)))
//                                    printfn "distance moved %A - %A = %A" startPos.Y (bBox.TopLeft.Y + (bBox.H / 2.0)) intDistance
//                                    makeSegImmov (moveSegment newModel wire4.Segments.[i] intDistance) i

//                          | Vertical ->
//                                if startPos.X < (bBox.TopLeft.X + (bBox.W / 2.0))
//                                then
//                                    let intDistance = -((bBox.W / 2.0) + 10.0 + (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))))
//                                    printfn "distance moved %A" intDistance
//                                    makeSegImmov (moveSegment newModel wire4.Segments.[i] intDistance) i
//                                else
//                                    let intDistance = (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))) - ((bBox.W / 2.0) + 10.0)
//                                    printfn "distance moved %A" intDistance
//                                    makeSegImmov (moveSegment newModel wire4.Segments.[i] intDistance) i

//                | None ->
//                          //printfn "Not moved"
//                          updateWire model (updateWire model wire4 false) true
                          
//            else
//                wire4

//        (wire3 ,[0..10]) ||> List.fold segFolder
    
//    (wire2, Map.toList symbols) ||> List.fold checkWireSegs

//    //autoroute model wire




let smartAutoroute (model: Model) (wire: Wire): Wire =
    printfn "-----------------New Autoroute-------------------"
    let wireExists = (List.tryFind (fun el -> snd(el).WId = wire.WId) (Map.toList model.Wires)) <> None
    let wire2 =     match wireExists with
                    | true  -> wire
                    | false -> printfn "Wire does not exist"
                               autoroute model wire
    let newModel =  match wireExists with
                    | true  -> model
                    | false -> updateWireSegmentJumps [wire2.WId] (Optic.set (wireOf_ wire2.WId) wire2 model)
        
    //printfn "Segments: %A" wire2.Segments
    //printfn "Wire has %A segments" (List.length wire2.Segments)
    let symbols = model.Symbol.Symbols
    

    let checkWireSegs (image: Wire*Model) (el: ComponentId*Symbol) =
        let wire3, model2 = image
        let symbol = snd(el)
        //printfn "Symbol %A" symbol.Id
        let segFolder (image2: Wire*Model) (i: int) =
            let wire4, model3 = image2
            if (i <= (List.length wire4.Segments) - 1) && (wire4.Segments.[i].Length <> 8.0)
            then
                let startPos, endPos = getAbsoluteSegmentPos wire4 i
                let bBox = getSymbolBoundingBox symbol

                let intersect = segOverSymbol symbol i wire4
                //printfn "Intersection %A segment %A" intersect i
                //printfn "startPos is %A" startPos

                let newWire =   match intersect with
                                | Some orientation ->
                                          printfn "Intersection %A segment %A" intersect i
                                          //let wire5 = makeSegsMovable wire4
                                          match orientation with
                                          | Horizontal ->
                                                if startPos.Y < (bBox.TopLeft.Y + (bBox.H / 2.0))
                                                then
                                                    let intDistance = -((startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0))) + (bBox.H / 2.0) + 10.0)
                                                    printfn "distance moved %A - %A = %A" startPos.Y (bBox.TopLeft.Y + (bBox.H / 2.0)) intDistance
                                                    makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i
                                                else
                                                    let intDistance = (bBox.H / 2.0) + 10.0 - (startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0)))
                                                    printfn "distance moved %A - %A = %A" startPos.Y (bBox.TopLeft.Y + (bBox.H / 2.0)) intDistance
                                                    makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i

                                          | Vertical ->
                                                if startPos.X < (bBox.TopLeft.X + (bBox.W / 2.0))
                                                then
                                                    let intDistance = -((bBox.W / 2.0) + 10.0 + (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))))
                                                    printfn "distance moved %A" intDistance
                                                    makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i
                                                else
                                                    let intDistance = (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))) - ((bBox.W / 2.0) + 10.0)
                                                    printfn "distance moved %A" intDistance
                                                    makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i

                                | None ->
                                          //printfn "Not moved"
                                          wire4 //updateWire model3 (updateWire model3 wire4 false) true
                newWire, updateModelWires model3 [newWire]
                          
            else
                let newWire = wire4 //updateWire model3 (updateWire model3 wire4 false) true
                newWire, updateModelWires model3 [newWire]
                

        ((wire3, model2) ,[0..10]) ||> List.fold segFolder
    
    fst(((wire2, newModel), Map.toList symbols) ||> List.fold checkWireSegs)