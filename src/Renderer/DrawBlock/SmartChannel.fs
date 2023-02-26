module SmartChannel

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

open SmartHelpers

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart channel route" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires in the BusWire model so could use the SmartHelper function for
    this purpose.
*)


///
/// HLP23: suggested initial smartChannel top-level function
/// to be tested, it must be given a channel in through which to route wires nicely
/// Normally the channel will come from symbol edges.
/// The function must identify all wires with segments going through the channel and space them
/// This function routes the parallel segment of all 7 segment wires by moving them perpendicular to its direction.
/// It is expected that all wires provided are 7 segment with parallel segment in the same direction
/// wires not so will be ignored.
/// The messiness of when to call it, what to do with different types of wire, which wires to call it with
/// could be left till later.
/// For this simple routing only one dimension of the channel is needed (determined by orientation).
/// The Wires going through the channel must be returned as an updated Wires map in model.
// HLP23: need to update XML doc comments when this function is fully worked out.

let testPrint x = printfn "filtered wires: %A" x; x

///The position of a segment relative to a channel
type PosRelativeToChannel = Inside | Outside


/// All possible wire directions, determined by the orientation of its input port
type Direction = Right | Left | Up | Down

//type FolderState = {
//    FirstSegPos: PosRelativeToChannel;
//    parallelSegEdges: (XYPos * XYPos) option;
//    LastSegPos: PosRelativeToChannel;
//    CurrentIndex: int
//}

/// Determines if a wire is part of the channel and returns the start and end positions of its parallel (towards the channel orientation) segment in the case that it is
/// The conditions for a wire to be considered part of the channel are:
///  -Its parallel segment must intersect the channel
///  -Either one of its edges must be located outide of the channel (i.e it must not be bounded by the channel)
let findParallelSegEdges (wire : Wire) (channel : BoundingBox) (parallelSegIndex: int)
        :(XYPos * XYPos) option =
    // Slightly shrinking the borders of the channel so that segments that are adjacent to it are considered exterior
    let adjustedChannel =  {channel with TopLeft = {X = channel.TopLeft.X + 8.0; Y = channel.TopLeft.Y + 8.0}; W = channel.W - 8.0; H = channel.H - 8.0}
    let segmentIntersectsChannel (segStart: XYPos) (segEnd: XYPos)
        (state: {|FirstSegPos: PosRelativeToChannel; parallelSegEdges: (XYPos * XYPos) option; LastSegPos: PosRelativeToChannel; CurrentIndex: int|})
        (seg: Segment) = 
        let distance: float option = segmentIntersectsBoundingBox adjustedChannel segStart segEnd
        if state.CurrentIndex = 0
        then
            match distance with
            | Some _ ->
                {|state with FirstSegPos = Inside; CurrentIndex = 1|}
            | None ->
                {|state with FirstSegPos = Outside; CurrentIndex = 1|}
        elif state.CurrentIndex = parallelSegIndex
        then
            match distance with
            | Some _ ->
                {|state with parallelSegEdges = Some (segStart, segEnd); CurrentIndex = state.CurrentIndex + 1|}
            | None  ->
                {|state with parallelSegEdges = None; CurrentIndex = state.CurrentIndex + 1|}
        elif state.CurrentIndex = List.length wire.Segments - 1
        then
            match distance with
            | Some _  ->
                {|state with LastSegPos = Inside; CurrentIndex = state.CurrentIndex + 1|} 
            | None ->
                {|state with LastSegPos = Outside; CurrentIndex = state.CurrentIndex + 1|} 
        else
            {|state with CurrentIndex = state.CurrentIndex + 1|}

    let initialState = {|FirstSegPos = Outside; parallelSegEdges = None; LastSegPos = Outside; CurrentIndex = 0|}
    let foldResult = foldOverSegs segmentIntersectsChannel initialState wire
    match foldResult.FirstSegPos, foldResult.LastSegPos, foldResult.parallelSegEdges with
    | Inside, Inside, _ -> None
    | _, _, Some edges -> Some edges
    | _ -> None

type ChannelWireInfo = {
    Wire: Wire;
    parallelSegIndex: int;
    parallelSegStartPos: XYPos;
    parallelSegEndPos: XYPos
    parallelSegOrientation: Orientation;
    Direction: Direction
}

let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    let tl = channel.TopLeft

    /// Check if a wire is part of the channel and returns a single element list with some wire metadata, mostly regarding the parallel segment if it is
    /// Intended for use alongside List.collect
    let extractParallelSegInfo (parallelSegIndex: int, wire: Wire) =
        match findParallelSegEdges wire channel parallelSegIndex with
        | Some edges ->
            let direction =
                match wire.Segments[0].Length > 0, wire.InitialOrientation with
                | true, Horizontal -> Right
                | true, Vertical -> Down
                | false, Horizontal -> Left
                | false, Vertical  ->  Up
            [{Wire = wire; parallelSegIndex = parallelSegIndex; parallelSegStartPos = fst edges; parallelSegEndPos = snd edges; parallelSegOrientation = findSegmentOrientation wire parallelSegIndex; Direction = direction}]
        | None -> []

    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    //Wire testing >>
    //model.Wires
    //|> Map.toList
    //|> List.map snd
    //|> List.sortBy (fun wire -> wire.StartPos.Y)
    //|> List.iter (fun wire ->
    //    let idx = findParallelSegmentIndexes wire
    //    //printfn $"Sgement number: {List.length wire.Segments}, parallelSegmentLength: {wire.Segments.[idx].Mode}, prevSegLegth: {wire.Segments.[idx - 1].Mode}, nextSegLegth: {wire.Segments.[idx + 1].Mode}")
    //    printfn $"Sgement number: {List.length wire.Segments} segments: {wire.Segments}")
    //Wire testing <<

    let channelWires =
       model.Wires
    |> Map.toList
    |> List.map (fun (_, wire) -> findParallelSegmentIndexes wire channelOrientation, wire)
    // Only keep wires with a single segment parallel to the channel (i.e candidates for rearrangement) to avoid overly complex scenarios
    |> List.collect (fun (parallelSegIndexes, wire) -> if List.length parallelSegIndexes = 1 then [(parallelSegIndexes.[0], wire)] else [])
    |> testPrint
    |> List.collect extractParallelSegInfo

    /// Move the parallel segment of the wires belonging to a group (wires have been grouped based on their starting position) in
    /// the direction perpendicular to the orientation of the channel
    let updateWireGroup (wireGroup: ChannelWireInfo list) (channelStartPosition: float) (distanceBetweenSegments: float) (wireGroupIndex: int) 
        : Wire List =
        let updateWire = function
        | {Wire = wire; parallelSegIndex = idx; parallelSegStartPos = startPos;} when channelOrientation = Horizontal->
            let moveDistance = channelStartPosition - startPos.Y + distanceBetweenSegments * (float wireGroupIndex + 1.0)
            moveSegment model wire.Segments.[idx] moveDistance
        | {Wire = wire; parallelSegIndex = idx; parallelSegStartPos = startPos;} ->
            let moveDistance = channelStartPosition - startPos.X + distanceBetweenSegments * (float wireGroupIndex + 1.0)
            moveSegment model wire.Segments.[idx] moveDistance

        wireGroup
        |> List.map updateWire

    /// Compares wire groups so that they are nicely ordered and spaced out in the channel
    /// Intended for use alongside List.sortWith
    let compareWireGroups (wireGroup1: {|parallelSegStart: float; WireInfo: ChannelWireInfo list; Direction: Direction|})
        (wireGroup2: {|parallelSegStart: float; WireInfo: ChannelWireInfo list; Direction: Direction|})
        :int =
        let findParallelSegmentLength (groupDirection: Direction) (wireInfo: ChannelWireInfo): float =
            match groupDirection with
            | Right | Down -> 
                (wireInfo.parallelSegEndPos.X - wireInfo.parallelSegStartPos.X + wireInfo.parallelSegEndPos.Y - wireInfo.parallelSegStartPos.Y) // No need to check for orientation since either Xs or Ys will cancel eachother
            | _ ->
                - (wireInfo.parallelSegEndPos.X - wireInfo.parallelSegStartPos.X + wireInfo.parallelSegEndPos.Y - wireInfo.parallelSegStartPos.Y)

        let longsetParallelSegLen1 =
           wireGroup1.WireInfo
        |> List.map (findParallelSegmentLength wireGroup1.Direction)
        |> List.maxBy abs
        let longsetParallelSegLen2 =
           wireGroup2.WireInfo
        |> List.map (findParallelSegmentLength wireGroup2.Direction)
        |> List.maxBy abs

        match longsetParallelSegLen1 > 0, longsetParallelSegLen2 > 0 with
        | true, true ->
            compare (- wireGroup1.parallelSegStart) (- wireGroup2.parallelSegStart)
        | true, false -> -1
        | false, true -> 1
        | _ ->
            compare wireGroup1.parallelSegStart wireGroup2.parallelSegStart

       // Grouped Version <<
       
    let updatedChannelWires =
        match channelOrientation with
        | Horizontal ->
            let groupedChannelWires =
               channelWires
            |> List.groupBy (fun parallelSegInfo -> parallelSegInfo.parallelSegStartPos.X, parallelSegInfo.Wire.OutputPort)
            |> List.map (fun ((startPosY, _), wireInfo) -> {|parallelSegStart = startPosY; WireInfo = wireInfo; Direction = wireInfo[0].Direction|})

            let distanceBetweenWires = channel.H / (float (List.length groupedChannelWires) + 1.0)

            groupedChannelWires
            |> List.sortWith compareWireGroups
            |> List.mapi (fun i wireGroupInfo -> updateWireGroup wireGroupInfo.WireInfo tl.Y distanceBetweenWires i)
            |> List.collect id
        | Vertical ->
            let groupedChannelWires =
               channelWires
            |> List.groupBy (fun parallelSegInfo -> parallelSegInfo.parallelSegStartPos.Y, parallelSegInfo.Wire.OutputPort)
            |> List.map (fun ((startPosX, _), wireInfo) -> {|parallelSegStart = startPosX; WireInfo = wireInfo; Direction = wireInfo[0].Direction|})

            let distanceBetweenWires = channel.W / (float (List.length groupedChannelWires) + 1.0)

            groupedChannelWires
            |> List.sortWith compareWireGroups
            |> List.mapi (fun i wireGroupInfo -> updateWireGroup wireGroupInfo.WireInfo tl.X distanceBetweenWires i)
            |> List.collect id

    updateModelWires model updatedChannelWires


    
