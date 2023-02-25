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
/// This function routes the middle segment of all 7 segment wires by moving them perpendicular to its direction.
/// It is expected that all wires provided are 7 segment with middle segment in the same direction
/// wires not so will be ignored.
/// The messiness of when to call it, what to do with different types of wire, which wires to call it with
/// could be left till later.
/// For this simple routing only one dimension of the channel is needed (determined by orientation).
/// The Wires going through the channel must be returned as an updated Wires map in model.
// HLP23: need to update XML doc comments when this function is fully worked out.

let testPrint x = printfn "filtered wires: %A" x; x

type PosRelativeToChannel = Inside | Outside

type FolderState = {
    FirstSegPos: PosRelativeToChannel;
    MiddleSegEdges: (XYPos * XYPos) option;
    LastSegPos: PosRelativeToChannel;
    CurrentIndex: int
}

/// Determines if a wire is part of the channel and returns the start and end positions of its middle segment in the case that it is
/// The conditions for a wire to be considered part of the channel are:
///  -Its middle segment must intersect the channel
///  -Either one of its edges must be located outide of the channel (i.e it must not be bounded by the channel)
let findMiddleSegEdges (wire : Wire) (channel : BoundingBox) (middleSegIndex: int)
        :(XYPos * XYPos) option =
    // Slightly shrinking the borders of the channel so that segments that are adjacent to it are considered exterior
    let extendedChannel =  {channel with TopLeft = {X = channel.TopLeft.X + 8.0; Y = channel.TopLeft.Y + 8.0}; W = channel.W - 8.0; H = channel.H - 8.0}
    let segmentIntersectsChannel (segStart: XYPos) (segEnd: XYPos) (state: FolderState) (seg: Segment) 
        : FolderState =
        let distance: float option = segmentIntersectsBoundingBox extendedChannel segStart segEnd
        if state.CurrentIndex = 0
        then
            match distance with
            | Some _ ->
                {state with FirstSegPos = Inside; CurrentIndex = 1}
            | None ->
                 {state with FirstSegPos = Outside; CurrentIndex = 1}
        elif state.CurrentIndex = middleSegIndex
        then
            match distance with
            | Some _ ->
                 {state with MiddleSegEdges = Some (segStart, segEnd); CurrentIndex = state.CurrentIndex + 1}
            | None  ->
                {state with MiddleSegEdges = None; CurrentIndex = state.CurrentIndex + 1}
        elif state.CurrentIndex = List.length wire.Segments - 1
        then
            match distance with
            | Some _  ->
                {state with LastSegPos = Inside; CurrentIndex = state.CurrentIndex + 1} 
            | None ->
                {state with LastSegPos = Outside; CurrentIndex = state.CurrentIndex + 1} 
        else
            {state with CurrentIndex = state.CurrentIndex + 1}

    let initialState: FolderState = {FirstSegPos = Outside; MiddleSegEdges = None; LastSegPos = Outside; CurrentIndex = 0}
    match (foldOverSegs segmentIntersectsChannel initialState wire) with
    | {FirstSegPos = Inside; LastSegPos = Inside;} -> None
    | {MiddleSegEdges = Some edges;} -> Some edges
    | _ -> None

type Direction = Right | Left | Up | Down

type ChannelWireInfo = {
    Wire: Wire;
    MidSegIndex: int;
    MidSegStartPos: XYPos;
    MidSegEndPos: XYPos
    MidSegOrientation: Orientation;
    Direction: Direction
}

let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    let tl = channel.TopLeft

    let filterWire (wire: Wire) (midSegIndex: int)
        :bool =
        (List.length wire.Segments > 6 || (wire.Segments[midSegIndex - 1].Length <> 0 || wire.Segments[midSegIndex + 1].Length <> 0)) && (findSegmentOrientation wire midSegIndex) = channelOrientation

    let extractMiddleSegInfo (midSegIndex: int, wire: Wire) =
        match findMiddleSegEdges wire channel midSegIndex with
        | Some edges ->
            let direction =
                match wire.Segments[0].Length > 0, wire.InitialOrientation with
                | true, Horizontal -> Right
                | true, Vertical -> Down
                | false, Horizontal -> Left
                | false, Vertical  ->  Up
            [{Wire = wire; MidSegIndex = midSegIndex; MidSegStartPos = fst edges; MidSegEndPos = snd edges; MidSegOrientation = findSegmentOrientation wire midSegIndex; Direction = direction}]
        | None -> []

    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"

    let channelWires =
       model.Wires
    |> Map.toList
    |> List.map (fun (_, wire) -> findMiddleSegmentIndex wire, wire)
    |> testPrint
    |> List.filter (fun (middleSegIndex, wire) ->  filterWire wire middleSegIndex)
    |> List.collect extractMiddleSegInfo
    // Grouped Version >>

    /// Moves the middle segment of the wires belonging to a group (wires have been grouped based on their starting position) in
    /// a direction perpendicular to the orientation of the channel
    let updateWireGroup (wireGroup: ChannelWireInfo list) (channelStartPosition: float) (distanceBetweenSegments: float) (wireGroupIndex: int) 
        : Wire List =
        let updateWire = function
        | {Wire = wire; MidSegIndex = idx; MidSegStartPos = startPos;} when channelOrientation = Horizontal->
            let moveDistance = channelStartPosition - startPos.Y + distanceBetweenSegments * (float wireGroupIndex + 1.0)
            moveSegment model wire.Segments.[idx] moveDistance
        | {Wire = wire; MidSegIndex = idx; MidSegStartPos = startPos;} ->
            let moveDistance = channelStartPosition - startPos.X + distanceBetweenSegments * (float wireGroupIndex + 1.0)
            moveSegment model wire.Segments.[idx] moveDistance

        wireGroup
        |> List.map updateWire

    // Grouped Version <<
    //let updateWire (wireInfo: MiddleSegInfo) (channelStartPosition: float) (distanceBetweenSegments: float) (wireGroupIndex: int)
    //    :Wire =
    //    match wireInfo with
    //    | {Wire = wire; Index = idx; StartPos = startPos; EndPos = _; Orientation = _} when channelOrientation = Horizontal->
    //        let moveDistance = channelStartPosition - startPos.Y + distanceBetweenSegments * (float wireGroupIndex + 1.0)
    //        moveSegment model wire.Segments.[idx] moveDistance
    //    | {Wire = wire; Index = idx; StartPos = startPos; EndPos = _; Orientation = _} ->
    //        let moveDistance = channelStartPosition - startPos.X + distanceBetweenSegments * (float wireGroupIndex + 1.0)
    //        moveSegment model wire.Segments.[idx] moveDistance

    // Grouped Version >>
    ///Compares wire groups so that they are nicely ordered and spaced out in the channel
    ///when sorted using List.sortWith

    let compareWireGroups (wireGroup1: {|MidSegStart: float; WireInfo: ChannelWireInfo list; Direction: Direction|})
        (wireGroup2: {|MidSegStart: float; WireInfo: ChannelWireInfo list; Direction: Direction|})
        :int =
        let findMidSegmentLength (groupDirection: Direction) (wireInfo: ChannelWireInfo): float =
            match groupDirection with
            | Right | Down -> 
                (wireInfo.MidSegEndPos.X - wireInfo.MidSegStartPos.X + wireInfo.MidSegEndPos.Y - wireInfo.MidSegStartPos.Y) // No need to check for orientation since either Xs or Ys will cancel eachother
            | _ ->
                - (wireInfo.MidSegEndPos.X - wireInfo.MidSegStartPos.X + wireInfo.MidSegEndPos.Y - wireInfo.MidSegStartPos.Y)

        let longsetMidSegLen1 =
           wireGroup1.WireInfo
        |> List.map (findMidSegmentLength wireGroup1.Direction)
        |> List.maxBy abs
        let longsetMidSegLen2 =
           wireGroup2.WireInfo
        |> List.map (findMidSegmentLength wireGroup2.Direction)
        |> List.maxBy abs

        match longsetMidSegLen1 > 0, longsetMidSegLen2 > 0 with
        | true, true ->
            compare (- wireGroup1.MidSegStart) (- wireGroup2.MidSegStart)
        | true, false ->
            compare (- wireGroup1.MidSegStart) wireGroup2.MidSegStart
        | false, true ->
            compare wireGroup1.MidSegStart (- wireGroup2.MidSegStart)
        | _ ->
            compare wireGroup1.MidSegStart wireGroup2.MidSegStart

       // Grouped Version <<
       
    //let compareWires (segStart1: float, wire1Info: MiddleSegInfo) (segStart2: float, wire2Info: MiddleSegInfo) 
    //    :int =
    //    let findSegmentLength (midSeg: MiddleSegInfo): float =
    //        (midSeg.EndPos.X - midSeg.StartPos.X + midSeg.EndPos.Y - midSeg.StartPos.Y) // No need to check for orientation since either Xs or Ys will cancel eachother

    //    let middleSeg1Len = findSegmentLength wire1Info
    //    let middleSeg2Len = findSegmentLength wire2Info

    //    match middleSeg1Len > 0 , middleSeg2Len > 0 with
    //    | true, true  ->
    //        compare (- segStart1) (- segStart2)
    //    | true, false ->
    //        compare segStart1 (- segStart2)
    //    | false, true ->
    //        compare (- segStart1) segStart2
    //    | _ ->
    //        compare segStart1 segStart2
    let updatedChannelWires =
        match channelOrientation with
        | Horizontal ->
            let groupedChannelWires =
               channelWires
            //|> List.map (fun (wire, middleSegIndex, segStart) -> (wire, middleSegIndex, segStart.X, segStart.Y, segStart.Y - (abs wire.Segments.[middleSegIndex - 1].Length), segStart.Y + (abs wire.Segments.[middleSegIndex + 1].Length)))
            |> List.groupBy (fun middleSegInfo -> middleSegInfo.MidSegStartPos.X, middleSegInfo.Wire.OutputPort)
            |> List.map (fun ((startPosY, _), wireInfo) -> {|MidSegStart = startPosY; WireInfo = wireInfo; Direction = wireInfo[0].Direction|})

            let distanceBetweenWires = channel.H / (float (List.length groupedChannelWires) + 1.0)

            groupedChannelWires
            |> List.sortWith compareWireGroups
            |> List.mapi (fun i wireGroupInfo -> updateWireGroup wireGroupInfo.WireInfo tl.Y distanceBetweenWires i)
            |> List.collect id
        | Vertical ->
            let groupedChannelWires =
               channelWires
            |> List.groupBy (fun middleSegInfo -> middleSegInfo.MidSegStartPos.Y, middleSegInfo.Wire.OutputPort)
            |> List.map (fun ((startPosX, _), wireInfo) -> {|MidSegStart = startPosX; WireInfo = wireInfo; Direction = wireInfo[0].Direction|})

            let distanceBetweenWires = channel.W / (float (List.length groupedChannelWires) + 1.0)
            //channelWires
            //|> List.map (fun segInfo -> (segInfo.StartPos.Y, segInfo))
            //|> List.sortWith compareWires 
            //|> List.mapi (fun i (_, wireInfo) -> updateWire wireInfo tl.X distanceBetweenWires i)
            groupedChannelWires
            //|> List.sortBy (fun (segStartY, _) -> segStartY)
            |> List.sortWith compareWireGroups
            |> List.mapi (fun i wireGroupInfo -> updateWireGroup wireGroupInfo.WireInfo tl.X distanceBetweenWires i)
            |> List.collect id

    updateModelWires model updatedChannelWires


    
