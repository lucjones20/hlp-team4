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

// HLP23: AUTHOR Gamaletsos (entire file)

/// Function to change all segments in a wire to automatic routing mode.
let makeSegsMovable (wire: Wire) :Wire =
    {
        wire with Segments =
                    wire.Segments |> List.map (fun seg -> {seg with Mode = Auto})
    }


/// Function to change the routing mode of one segment to manual.
/// This function returns a copy of the wire with the change on the corresponding segment.
let makeSegImmov (wire: Wire) (i: int): Wire =
    let newSegs = wire.Segments |> List.map ( fun seg ->
        if seg.Index = i
        then {seg with Mode = Manual}
        else seg )
    {wire with Segments = newSegs}


/// Small helper to remove the first and last elements from a list.
/// It returns a new copy of the list without these two elements.
let removeFstLast list =
    list |> List.tail |> List.rev |> List.tail |> List.rev


/// This is closely resembling the updateWire function from BusWireUpdate.
/// The intention was to use it to partially autoroute the edges of the wires.
/// The edges of the wires are connected to components that move on the screen.
/// When a component moves, the wire should re-route.
/// PartialAutoroute routes the wire from its start to the first manually routed segment,
/// hence it would solve the problem of routing the edges of the wires that do not
/// intersect with any symbols. 
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


/// SmartAutoroute will route the newly connected wires around symbols.
/// When a new wire is created, autoroute will give it its basic form,
/// that means all the initial vertices and segment lengths.
/// After the wire is created, smartAutoroute's functionality will
/// move the segments of the wires that intersect with any symbols.
/// When a segment does not intersect a symbol, it should be autoRouted normally.
/// The segments that do intersect with symbols should stay in their
/// new position that was given by the symbol avoidance functionality.
let smartAutoroute (model: Model) (wire: Wire): Wire = 
    autoroute model wire


// let smartAutoroute (model: Model) (wire: Wire): Wire =

//     // If a wire does not exist yet, it must first be created by the autoroute function.
//     // If a wire already exists, it is handled as is.
//     let wireExists = (List.tryFind (fun el -> snd(el).WId = wire.WId) (Map.toList model.Wires)) <> None
//     let wire2 =     match wireExists with
//                     | true  -> wire
//                     | false -> printfn "Wire does not exist"
//                                autoroute model wire
//     // When a new wire is created, the model must be updated before any further changes are made to the wire.
//     let newModel =  match wireExists with
//                     | true  -> model
//                     | false -> updateWireSegmentJumps [wire2.WId] (Optic.set (wireOf_ wire2.WId) wire2 model)
        
//     let symbols = model.Symbol.Symbols
    

//     // This function folds over the symbols of the model.
//     // Since it is a folder function, it needs to keep track of the state of the model and the wire.
//     let symbolFolder (image: Wire*Model) (el: ComponentId*Symbol) =
//         let wire3, model2 = image
//         let symbol = snd(el)

//         // This function folds over the segments of the wire.
//         // It checks if any of the segments intersect with a given symbol from the model.
//         // If a segment intersects, its position will be changed.
//         // This function, like symbolFolder, needs to keep track of the state of the model and the wire.
//         let segFolder (image2: Wire*Model) (i: int) =
//             let wire4, model3 = image2
//             if (i <= (List.length wire4.Segments) - 1) && (wire4.Segments.[i].Length <> 8.0)
//             then
//                 let startPos, endPos = getAbsoluteSegmentPos wire4 i
//                 let bBox = getSymbolBoundingBox symbol
//                 let intersect = segOverSymbol symbol i wire4

//                 let newWire =   match intersect with
//                                 | Some orientation ->
//                                           match orientation with
//                                           | Horizontal ->
//                                                 if startPos.Y < (bBox.TopLeft.Y + (bBox.H / 2.0))
//                                                 then
//                                                     let intDistance = -((startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0))) + (bBox.H / 2.0) + 10.0)                                                
//                                                     makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i
//                                                 else
//                                                     let intDistance = (bBox.H / 2.0) + 10.0 - (startPos.Y - (bBox.TopLeft.Y + (bBox.H / 2.0)))                                                   
//                                                     makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i

//                                           | Vertical ->
//                                                 if startPos.X < (bBox.TopLeft.X + (bBox.W / 2.0))
//                                                 then
//                                                     let intDistance = -((bBox.W / 2.0) + 10.0 + (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))))                                                   
//                                                     makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i
//                                                 else
//                                                     let intDistance = (startPos.X - (bBox.TopLeft.X + (bBox.W / 2.0))) - ((bBox.W / 2.0) + 10.0)
//                                                     makeSegImmov (moveSegment model3 wire4.Segments.[i] intDistance) i

//                                 | None -> updateWire model3 (updateWire model3 wire4 false) true

//                 newWire, updateModelWires model3 [newWire]
                          
//             else
//                 let newWire = updateWire model3 (updateWire model3 wire4 false) true
//                 newWire, updateModelWires model3 [newWire]
                

//         ((wire3, model2) ,[0..10]) ||> List.fold segFolder
    
//     fst(((wire2, newModel), Map.toList symbols) ||> List.fold symbolFolder)