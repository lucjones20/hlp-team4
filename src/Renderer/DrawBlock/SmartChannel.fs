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


///The position of a point relative to a channel
type PosRelativeToChannel = Inside | Outside

/// Represents a wire which is part of a channel and is thus a candidate for rearrangement.
/// Holds extra information regarding its segment that is parallel to the channel, i.e the segment that will be shifted
/// when wires are spaced out
type ChannelWire = {
    Wire: Wire;
    ParallelSegIndex: int;
    ParallelSegStartPos: XYPos;
    ParallelSegOrientation: Orientation
}

/// Takes in a subChannel (determnied in the smartChannelRoute main function) along with the channel wires which intersect this
/// subChannel and spaces them out nicely, so that the wires are clearly visible and intersections are avoided (to an extent)
let routeSubChannelWires
    (channelOrientation: Orientation) 
    (subChannel: BoundingBox)
    (subChannelWires: ChannelWire list)
    (model:Model) 
        :Model =

    /// Move the parallel segment of the channel wires belonging to a group (wires have been grouped based on the starting position of their parallel
    /// segments and their output ports) in the direction perpendicular to the orientation of the channel
    let updateWireGroup (wireGroup: ChannelWire list) (channelStartPosition: float) (separationDistance: float) (wireGroupIndex: int) 
        : Wire List =
        let updateWire = function
        | {Wire = wire; ParallelSegIndex = idx; ParallelSegStartPos = startPos;} when channelOrientation = Horizontal->
            let moveDistance = channelStartPosition - startPos.Y + separationDistance * (float wireGroupIndex + 1.0)
            moveSegment model wire.Segments.[idx] moveDistance
        | {Wire = wire; ParallelSegIndex = idx; ParallelSegStartPos = startPos;} ->
            let moveDistance = channelStartPosition - startPos.X + separationDistance * (float wireGroupIndex + 1.0)
            moveSegment model wire.Segments.[idx] moveDistance

        wireGroup
        |> List.map updateWire

    /// Compares wire groups so that they are nicely ordered and spaced out in the sub channel
    /// Intended for use alongside List.sortWith
    let compareWireGroups (wireGroup1: {|ParallelSegStart: float; Wires: ChannelWire list|})
        (wireGroup2: {|ParallelSegStart: float; Wires: ChannelWire list|})
        :int =
        let findSegmentLength (segmentIndex: int) (cw: ChannelWire): float =
            cw.Wire.Segments.[segmentIndex].Length
        // Find the wire with the longest parallel segment in each group
        // Since all wires in the wire group will be moved/shifted together height/width (depending on orientation)
        // do the sorting based ot its longest parallel segment since its the one with the potential to cross
        // over the most wires
        let wireWithLongestSeg1 =
           wireGroup1.Wires
        |> List.maxBy (fun channelWire -> abs (findSegmentLength channelWire.ParallelSegIndex channelWire))
        let wireWithLongestSeg2 =
           wireGroup2.Wires
        |> List.maxBy (fun channelWire ->abs (findSegmentLength channelWire.ParallelSegIndex channelWire))

        let parallelAndPrevSameSign1  = wireWithLongestSeg1.Wire.Segments.[wireWithLongestSeg1.ParallelSegIndex].Length * wireWithLongestSeg1.Wire.Segments.[wireWithLongestSeg1.ParallelSegIndex - 1].Length > 0
        let parallelAndPrevSameSign2  = wireWithLongestSeg2.Wire.Segments.[wireWithLongestSeg2.ParallelSegIndex].Length * wireWithLongestSeg2.Wire.Segments.[wireWithLongestSeg2.ParallelSegIndex - 1].Length > 0
        match parallelAndPrevSameSign1 , parallelAndPrevSameSign2  with
        | true, true ->
            compare (- wireGroup1.ParallelSegStart) (- wireGroup2.ParallelSegStart)
        | true, false -> -1
        | false, true -> 1
        | _ ->
            compare wireGroup1.ParallelSegStart wireGroup2.ParallelSegStart

    let tl = subChannel.TopLeft
    let updatedChannelWires =
        match channelOrientation with
        | Horizontal ->
            let groupedChannelWires =
               subChannelWires
            |> List.groupBy (fun channelWire -> channelWire.ParallelSegStartPos.X, channelWire.Wire.OutputPort)
            |> List.map (fun ((startPosX, _), channelWires) -> {|ParallelSegStart = startPosX; Wires = channelWires|})

            let separationDistance = subChannel.H / (float (List.length groupedChannelWires) + 1.0)
            groupedChannelWires
            |> List.sortWith compareWireGroups
            |> List.mapi (fun i wireGroup -> updateWireGroup wireGroup.Wires tl.Y separationDistance i)
            |> List.collect id
        | Vertical ->
            let groupedChannelWires =
               subChannelWires
            |> List.groupBy (fun channelWire -> channelWire.ParallelSegStartPos.Y, channelWire.Wire.OutputPort)
            |> List.map (fun ((startPosY, _), channelWires) -> {|ParallelSegStart = startPosY; Wires = channelWires|})

            let separationDistance = subChannel.W / (float (List.length groupedChannelWires) + 1.0)
            groupedChannelWires
            |> List.sortWith compareWireGroups
            |> List.mapi (fun i wireGroup -> updateWireGroup wireGroup.Wires tl.X separationDistance i)
            |> List.collect id

    updateModelWires model updatedChannelWires


/// Takes in the wires that have been assigned to the channel, and creates sub channels based on which
/// wires have overlapping parallel segments
/// Wires will then be rearranges seperatly for each sub channel which will result to better spacing between wires
let formSubChannels
    (channelWires: ChannelWire list)
    (channel: BoundingBox)
    (channelOrientation: Orientation)
        :(BoundingBox * ChannelWire list) list =
    /// Find the range covered by the parallel segment of a channel wire
    /// and return it in the form of a start and end position pair
    let findParallelSegRange (wire: ChannelWire) =
        let parallelSegLength = wire.Wire.Segments.[wire.ParallelSegIndex].Length
        match parallelSegLength > 0, channelOrientation with
        | true, Vertical ->
            {|Start = wire.ParallelSegStartPos.Y; End = wire.ParallelSegStartPos.Y + parallelSegLength|}
        | true, _ ->
            {|Start = wire.ParallelSegStartPos.X; End = wire.ParallelSegStartPos.X + parallelSegLength|}
        | _, Vertical->
            {|Start = wire.ParallelSegStartPos.Y + parallelSegLength; End = wire.ParallelSegStartPos.Y|}
        | _ ->
            {|Start = wire.ParallelSegStartPos.X + parallelSegLength; End = wire.ParallelSegStartPos.X|}

    /// Fold over the sorted (based on start position) list of ranges and return a list of ranges
    /// which is formed by superimposing the input ranges
    /// The ouput ranges represent the width/height (based on channel orientation) of the sub channel boundibg boxes
    let findSubChannelRanges (sortedRanges: {|Start: float; End: float|} list) =
        let superimposeRanges (state: {|currRange: {|Start: float; End: float|} ; superimposedRanges: {|Start: float; End: float|} list|})
            (range: {|Start: float; End: float|}) = 
            if state.currRange.End >= range.Start
            then
                let currRange' = {|Start = state.currRange.Start; End = max (state.currRange.End) range.End|}
                {|state with currRange = currRange'|}
            else
                {|currRange = range; superimposedRanges = state.currRange::state.superimposedRanges|}
        let foldResult = 
           sortedRanges
        |> List.tail
        |> List.fold superimposeRanges {|currRange = List.head sortedRanges; superimposedRanges = []|}
        
        (foldResult.currRange)::(foldResult.superimposedRanges)

    /// Allocate channel wires to a range (i.e sub channel) and return the
    /// sub channel BoundingBox which corresponds to that range alongisde a list of the allocated wires
    let allocateWires (channelWires: ChannelWire list) (range: {|Start: float; End: float|})
        :BoundingBox * ChannelWire list =
        let allocatedWires = 
           ([], channelWires)
        ||> List.fold (fun (allocatedWires: ChannelWire list) (wire: ChannelWire) ->
                  match channelOrientation with
                  | Vertical ->
                        if wire.ParallelSegStartPos.Y <= range.End &&  wire.ParallelSegStartPos.Y >= range.Start
                        then wire::allocatedWires
                        else allocatedWires
                  | _ ->
                        if wire.ParallelSegStartPos.X <= range.End &&  wire.ParallelSegStartPos.X >= range.Start
                        then wire::allocatedWires
                        else allocatedWires)

        let (subChannel: BoundingBox) =
            match channelOrientation with
            | Vertical ->
                let tl = {X = channel.TopLeft.X; Y = max channel.TopLeft.Y range.Start}
                {TopLeft = tl; W = channel.W; H = min range.End (channel.TopLeft.Y + channel.H) - tl.Y}
            | _ ->
                let tl = {X = max channel.TopLeft.X range.Start; Y = channel.TopLeft.Y }
                {TopLeft = tl; W = min range.End (channel.TopLeft.X + channel.W) - tl.X; H = channel.H}

        subChannel , allocatedWires
                                
    //let pipePrint (x: {|Start: float; End: float|} list) =
    //    x
    //    |> List.iter (fun x -> printfn $"{x.Start}, {x.End}")
    //    printfn "############################"
    //    x

    channelWires
    |> List.map findParallelSegRange
    |> List.sortBy (fun range -> range.Start)
    |> findSubChannelRanges
    |> List.fold (fun subChannels range-> (allocateWires channelWires range)::subChannels) []

/// Determine if a wire is part of the channel and return the start and end positions of its parallel (towards the channel orientation) segment in the case that it is
/// The conditions for a wire to be considered part of the channel are:
///  -Its parallel segment must intersect the channel
///  -Either one of its edges must be located outside of the channel (i.e it must not be bounded by the channel)
let findParallelSegEdges
    (wire : Wire)
    (channel : BoundingBox)
    (parallelSegIndex: int)
        :(XYPos * XYPos) option =
    let pointInsideBoundingBox (point: XYPos) (bb: BoundingBox)
        :bool =
        let horizontallyBound = point.X < bb.TopLeft.X + bb.W && point.X > bb.TopLeft.X
        let verticallyBound = point.Y < bb.TopLeft.Y + bb.H && point.Y > bb.TopLeft.Y
        horizontallyBound && verticallyBound
    /// Folder fuction to be passed into foldOverSegs
    /// The state it passes through foldOverSegs collects information regarding whether the wire should be included in the channel
    let collectWireInfo (segStart: XYPos) (segEnd: XYPos)
        (state: {|WireStartPos: PosRelativeToChannel; parallelSegEdges: (XYPos * XYPos) option; WireEndPos: PosRelativeToChannel; CurrentIndex: int|})
        (seg: Segment) = 
        if state.CurrentIndex = 0
        then
            match pointInsideBoundingBox segStart channel with
            | true ->
                {|state with WireStartPos = Inside; CurrentIndex = 1|}
            | _ ->
                {|state with WireStartPos = Outside; CurrentIndex = 1|}
        elif state.CurrentIndex = parallelSegIndex
        then
            let distance: float option = segmentIntersectsBoundingBox channel segStart segEnd
            match distance with
            | Some _ ->
                {|state with parallelSegEdges = Some (segStart, segEnd); CurrentIndex = state.CurrentIndex + 1|}
            | None  ->
                {|state with parallelSegEdges = None; CurrentIndex = state.CurrentIndex + 1|}
        elif state.CurrentIndex = List.length wire.Segments - 1
        then
            match pointInsideBoundingBox segEnd channel with 
            | true ->
                {|state with WireEndPos = Inside; CurrentIndex = state.CurrentIndex + 1|} 
            | _ ->
                {|state with WireEndPos = Outside; CurrentIndex = state.CurrentIndex + 1|} 
        else
            {|state with CurrentIndex = state.CurrentIndex + 1|}

    let initialState = {|WireStartPos = Outside; parallelSegEdges = None; WireEndPos = Outside; CurrentIndex = 0|}
    let foldResult = foldOverSegs collectWireInfo initialState wire
    match foldResult.WireStartPos, foldResult.WireEndPos, foldResult.parallelSegEdges with
    | Inside, Inside, _ -> None
    | _, _, Some edges -> Some edges
    | _ -> None

/// This function is given a channel, (Vertical or Horizontal) and first identifies wires that 
/// that should be rerouted, in order to make the channel clearer and cleaner.
/// The conditions for a wire to be considered for rerouting are that it should firstly pass
/// through the channel, have a segment parallel to the channel oriantation that goes through the channel
/// and also have at least one edge originating from outsdie the channel. Both autorouted and manually routed wire and
/// are considered
/// The segment parallel to the channel is the one which will be moved to achieve this better spacing, and for this implementation
/// only wires with one non zero length segment will be included.
/// The idea behind the algorithm is to divide the channel into sub channels where the parallel segments of the previously selected wires
/// overlap either horizontally or vertically (depending on channel orientation) and space out the wires nicely and evenly
/// within each sub channel
let smartChannelRoute 
    (channelOrientation: Orientation) 
    (channel: BoundingBox) 
    (model:Model) 
        :Model =

    /// Check if a wire is part of the channel and if it is, returns a list with a single channelWire
    /// Intended for use alongside List.collect
    let findChannelWires (parallelSegIndex: int, wire: Wire)
        :ChannelWire list=
        match findParallelSegEdges wire channel parallelSegIndex with
        | Some edges ->
            [{Wire = wire; ParallelSegIndex = parallelSegIndex; ParallelSegStartPos = fst edges; ParallelSegOrientation = findSegmentOrientation wire parallelSegIndex}]
        | None -> []

    //printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    let channelWires =
       model.Wires
    |> Map.toList
    |> List.map (fun (_, wire) -> findParallelSegmentIndexes wire channelOrientation, wire)
    // Only keep wires with a single segment parallel to the channel (i.e candidates for rearrangement) to avoid overly complex scenarios
    |> List.collect (fun (parallelSegIndexes, wire) -> if List.length parallelSegIndexes = 1 then [(parallelSegIndexes.[0], wire)] else [])
    |> List.collect findChannelWires

    let subChannels = formSubChannels channelWires channel channelOrientation

    (model, subChannels)
    ||> List.fold (fun model subChannel -> routeSubChannelWires channelOrientation (fst subChannel) (snd subChannel) model) 
