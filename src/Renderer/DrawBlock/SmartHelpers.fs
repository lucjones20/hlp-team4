module SmartHelpers

open Elmish
open Fulma
open Fable.React.Props
open Fable.React
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open Symbol

open Optics
open Operators


//-----------------------------------------------------------------------------------------------//
//---------------------------HELPERS FOR SMART DRAW BLOCK ADDITIONS------------------------------//
//-----------------------------------------------------------------------------------------------//

(*
HOW TO USE THIS MODULE.

(1) Add well-documented useful functions - see updateModelSymbols and updateModelWires
    for examples. You do not need to add performance information as in updateModelSymbols. 
    Your priority should be writing clear code. Try to avoid very inefficient implementations
    if possible (e.g. 100X slower than a similar complexity solution), but do not worry 
    about this.
(2) Note from my examples distinction between XML documentation and additional details
    in header comments.
(3) HLP23: Note comments here labelled "HLP23" which are for HLP23 class and would be deleted in
    production (Group phase) code.
(2) HLP23: Each function must have a single author specified by "HLP23: AUTHOR" in an XML comment
    as in my example: give name as Family name only (unique within teams).
(3) HLP23: Inform other members that you have written a function they could maybe use.
(4) HLP23: If two people end up with near-identical functions here team phase can rationalise if
    needed normally you are expected to share when this makes code writing faster.
(5) Note best practice here using Optics for nested record update. This is NOT curently required
    in Issie but used appropriately results in better code. Use it if you are comfortable doing so.
(5) Note on qualifying types: do this when not doing it would be ambiguous - e.g. here
    the BusWire and Symbol Model types.
(6) Note on code layout. A limit of 100 characters per line is used here. Seems about right.
*)

//----------------------------------------------------------------------------------------------//

/// Update BusWire model with given symbols. Can also be used to add new symbols.
/// This uses a fold on the Map to add symbols which makes it fast in the case that the number
/// of symbols added is very small.
//  Performance scales as O(M*log2(N)) - M = number of symbols added, N = number of existing
//  Symbols. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelSymbols 
    (model: BusWireT.Model) 
    (symbols: Symbol list)
        : BusWireT.Model =
    // HLP23: note on fold implementation. symMap is both argument and result of the
    // fold function => sequential set of updates. In thsi case much more efficient than Map.map
    // over all symbols.
    // HLP23 - see also similar updateModelWires
    let symbols' =
        (model.Symbol.Symbols,symbols)
        ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap)
    Optic.set (symbol_ >-> symbols_) symbols' model

/// Update BusWire model with given wires. Can also be used to add new wires.
/// This uses a fold on the Map to add wires which makes it fast in the case that the number
/// of wires added is small.
//  Performance scales as O(M*log2(N)) - M = number of wires added, N = number of existing
//  wires. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelWires 
    (model: BusWireT.Model) 
    (wiresToAdd: Wire list)
        : BusWireT.Model =
    //
    // HLP23: note on fold implementation. In this (typical) example Map is
    // sequentially updated by the fold. A common and difficult to see coding mistake is to use the 
    // original wireMap (argument of Optic map function) not the updated one (wireMap argument of 
    // List.map folder) in the fold function! That is not possible here because both have the same 
    // name so the inner bound updated wireMap is always what is used in the folder function. 
    // This is good practice, and if you have ever debugged this type of mistake you will know it
    // is very necessary!

    // HLP23: note on this use of Optics.map in a pipeline. It is more "functional" than the 
    // equivalent implementation using a let definition and Optics.set. Is it clearer? Or less clear? 
    // Standard logic says we should prefer the pipeline if the name of the let definition adds 
    // nothing which is the case here. I have given you both ways of using Optics here so you can 
    // compare the two implementations and decide. NB - you are NOT required to use Optics in your 
    // own code.
    //
    // HLP23: Note how multiple updates to different parts of the model can be neatly pipelined 
    // like this using a separate Optic.map or Optic.set for each.
    //
    // HLP23: note that if the operation here was larger or part of some pipeline the
    // 2nd argument to Optic.map - which defines the model change - could be given a name and 
    // turned into a local function making the Optic.map line like:
    // |> Optic.map wires_ myNameForThisWireMapUpdate
    model
    |> Optic.map wires_ (fun wireMap  ->
        (wireMap,wiresToAdd)
        ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))


module Constants = 
    let portTextCharWidth = 8.


/// Find the orientation of a wire segment given its index
///HLP23: AUTHOR Sougioultzoglou
let findSegmentOrientation (wire: Wire) (segmentIndex: int)
    : Orientation =
    match segmentIndex % 2 with
    | 1 when wire.InitialOrientation = Vertical -> Horizontal
    | 1 -> Vertical
    | _ -> wire.InitialOrientation

/// Find the Indexes of manually routed segments in a wire
/// HLP23: AUTHOR Sougioultzoglou
let findManualSegmentIndexes (wire: Wire) 
    :int list =
    wire.Segments
    |> List.fold (fun manualIndexes  seg ->
        if seg.Mode = Manual then seg.Index::manualIndexes else manualIndexes) []


/// Find the indexes of a wires segments that are parallel to a given orientation and
/// have some slack room to be moved in the direction perpendicular to the orientation
/// Doesn't include nubs, 0 length segments and segments that are attached to 
/// and ligned with the the input and output ports (since they can't be elegantly moved)
/// HLP23: AUTHOR Sougioultzoglou
let findParallelSegmentIndexes (wire: Wire) (orientation: Orientation)=
    let zeroLengthSegsRemoved = 
       wire.Segments
    |> List.filter (fun seg -> seg.Length <> 0)

    zeroLengthSegsRemoved[2..List.length zeroLengthSegsRemoved - 3]
    |> List.fold (fun indexes seg ->
        if findSegmentOrientation wire seg.Index = orientation
        then seg.Index::indexes
        else  indexes) []


/// Returns a distance for a wire move that has been reduced if needed to
/// keep the segment edges that need to be moved curved, as well as enforce minimum first/last segment lengths
let getSaferDistanceForMove (segments: Segment list) (index: int) (distance: float) =
    /// Returns a list of segments up to the first non-zero segment perpendicular to the segment leaving the port
    let findBindingSegments portIndex (segList: Segment List) = 
        segList
        |> List.takeWhile (fun seg -> seg.Index % 2 = portIndex % 2 || seg.Length = 0) // Works for both input and output ports

    let findDistanceFromPort boundSegList =
        (0., boundSegList)
        ||> List.fold (fun dist seg -> dist + seg.Length) // Since the segments in perpendicular direction are 0 we can just sum up all the segments as if they are in the same direction
   
    let reduceDistance bindingSegs findBindingIndex distance = 
        if findBindingIndex bindingSegs <> index then 
            distance
        else
            findDistanceFromPort bindingSegs
            |> (fun dist -> 
                    if sign dist = -1 then 
                        max distance (dist + Constants.nubLength + Constants.cornerRadius)
                    else 
                        min distance (dist - Constants.nubLength - Constants.cornerRadius))

    let bindingInputSegs = 
        segments
        |> findBindingSegments 0
        |> List.map (fun seg -> { seg with Length = -seg.Length})

    let bindingOutputSegs =
        List.rev segments
        |> findBindingSegments (segments.Length - 1)

    let findInputBindingIndex boundSegList =
        boundSegList
        |> List.length

    let findOutputBindingIndex =
        findInputBindingIndex
        >> (-) (segments.Length - 1)

    distance
    |> reduceDistance bindingInputSegs findInputBindingIndex
    |> reduceDistance bindingOutputSegs findOutputBindingIndex


/// Returns a wwireOf_aining the updated list of segments after a segment is moved by 
/// a specified distance. The moved segment is tagged as manual so that it is no longer auto-routed.
/// This function is very similar to moveSegment but restricts the moved distance so as not to cut of any rounded
/// segment corners
/// Throws an error if the index of the segment being moved is not a valid movable segment index.
let moveSegmentRestricted (model:Model) (seg:Segment) (distance:float) = 
    let wire = model.Wires[seg.WireId]
    let segments = wire.Segments
    let idx = seg.Index

    if idx <= 0 || idx >= segments.Length - 1 then // Should never happen
        printfn $"Trying to move wire segment {seg.Index}:{logSegmentId seg}, out of range in wire length {segments.Length}"
        wire
    else
        let safeDistance = getSaferDistanceForMove segments idx distance
    
        let prevSeg = segments[idx - 1]
        let nextSeg = segments[idx + 1]
        let movedSeg = segments[idx]

        let newPrevSeg = { prevSeg with Length = prevSeg.Length + safeDistance }
        let newNextSeg = { nextSeg with Length = nextSeg.Length - safeDistance }
        let newMovedSeg = { movedSeg with Mode = Manual }
    
        let newSegments = 
            segments[.. idx - 2] @ [newPrevSeg; newMovedSeg; newNextSeg] @ segments[idx + 2 ..]

        { wire with Segments = newSegments }



/// sort 2 symbols with respect to their X position
/// the left most symbol will be the first element in the return tuple
/// HLP23: AUTHOR Jones
let sortSymbolsLeftToRight (s1: Symbol) (s2: Symbol): Symbol * Symbol= 
    if (toX s1.Pos < toX s2.Pos) then s1, s2
    else s2, s1

/// returns a map of the wires connected from s1 to s2
/// note that the order of the symbols matters: s1 is the output and s2 is the input
/// HLP23: AUTHOR Jones
let getSelectedSymbolWires (wModel: BusWireT.Model) (s1: Symbol) (s2: Symbol): Map<ConnectionId, Wire> = 
    let matchInputOutputPorts key value : bool= 
        ((s1.Component.OutputPorts
        |> List.map (fun (x:Port) -> x.Id)
        |> List.contains (string value.OutputPort)) // check that one of the left symbol's output ports is the wire's output port
        && ( s2.Component.InputPorts
        |> List.map (fun (x:Port) -> x.Id)
        |> List.contains (string value.InputPort)))
        ||
        ((s2.Component.OutputPorts
        |> List.map (fun (x:Port) -> x.Id)
        |> List.contains (string value.OutputPort)) // check that one of the left symbol's output ports is the wire's output port
        && ( s1.Component.InputPorts
        |> List.map (fun (x:Port) -> x.Id)
        |> List.contains (string value.InputPort)))
        //  //check that one of the right symbol's input ports is the wire's input port
    wModel.Wires
    |> Map.filter matchInputOutputPorts

let getSelectedEdgeWires (wModel: BusWireT.Model) (s1: Symbol) (s2: Symbol) (e1: Edge) (e2: Edge): Map<ConnectionId, Wire> =
    let matchEdgeToEdgePorts key value : bool = 
        let edge1Wires = Map.find e1 s1.PortMaps.Order
        let edge2Wires = Map.find e2 s2.PortMaps.Order
        ((List.contains (string value.OutputPort) edge1Wires || List.contains (string value.InputPort) edge1Wires)
        &&
        (List.contains (string value.OutputPort) edge2Wires) || List.contains (string value.InputPort) edge2Wires)
    wModel.Wires
    |> Map.filter matchEdgeToEdgePorts


/// lenses used to edit symbols 
/// HLP23: should be placed in DrawModelType and CommonTypes (I didn't want to change too many files so they are 
/// here for now)
/// HLP23: AUTHOR Jones
let pos_: Lens<Symbol,XYPos> = Lens.create (fun a -> a.Pos) (fun s a -> {a with Pos = s}) // change Pos of Symbol

let x_: Lens<XYPos,float>= Lens.create (fun a -> a.X) (fun s a -> {a with X = s}) // change Y of XYPos

let y_: Lens<XYPos,float> = Lens.create (fun a -> a.Y) (fun s a -> {a with Y = s}) // change X of XYPos

let labelBoundingBox_: Lens<Symbol, BoundingBox> = // change LabelBoundingBox of Symbol
    Lens.create (fun a -> a.LabelBoundingBox) (fun s a -> {a with LabelBoundingBox = s}) 

let topLeft_: Lens<BoundingBox, XYPos> = Lens.create (fun a -> a.TopLeft) (fun s a -> {a with TopLeft = s}) // change TopLeft of LabelBoundingBox

let hScale_: Lens<Symbol, float option> = Lens.create (fun a -> a.HScale) (fun s a -> {a with HScale = s})

let vScale_: Lens<Symbol, float option> = Lens.create (fun a -> a.VScale) (fun s a -> {a with VScale = s})

/// this functions takes two lists: the first one is the original list where a sublist is ordered wrong and the second
/// is the correct ordering of that sublist 
/// this function will return a list starting with the order of the original list and once it hits an element in the sublist
/// the return list's order will be the desired order of the sublist (note that the elements in the sublist are now all grouped up
/// in the return list, which is not necessarily the case in the original list)
/// if there are any elements remaining from the original list not yet added to the return list, the order of the return list will
/// go back to the ordering of the original list  
/// example to illustrate: 
///     correctOrderingOfList [a; b; c; d; e] [c; d; b] --> [a; c; d; b; e]
///     correctOrderingOfList [a; b; c; d; e] [e; b; d] --> [a; e; b; d; c]
/// HLP23: AUTHOR Jones
let correctOrderingOfList (originalList: string list) (correctOrderList: string list): string list = 
    let wrongPorts = List.except correctOrderList originalList 
    let rec assembleList wrongPortList correctOrderList index acc = 
        match wrongPortList with
            |a::tail -> if originalList[index] = a then assembleList tail correctOrderList (index+1) (acc @ [a])
                        else acc @ correctOrderList @ wrongPortList
            | _ -> acc @ correctOrderList
    assembleList wrongPorts correctOrderList 0 []

/// this function checks to see if all the wires between 2 symbols have an initial orientation of Horizintal
/// note: the ordering of the symbols matter, s1 needs to have the output ports and s2 needs to have the input ports
/// HLP23: AUTHOR Jones
let hasHorizontalWires (wModel: BusWireT.Model) (s1: Symbol) (s2: Symbol): bool = 
    let selectedWires = getSelectedSymbolWires wModel s1 s2
    (false, (Map.values selectedWires
    |> Seq.map (fun x -> x.InitialOrientation = Horizontal)))
    ||> Seq.fold (||)

// sortSymbolByOutputToInput
let sortSymbolByOutputToInput (wModel: BusWireT.Model) (s1: Symbol) (s2: Symbol): Symbol * Symbol = 
    let selectedWires = getSelectedSymbolWires wModel s1 s2
    selectedWires
    |> Map.values
    |> Seq.head
    |> fun x -> (x.OutputPort, x.InputPort)
    |> fun (x,y) -> (Map.tryFind (string x) s1.PortMaps.Orientation, Map.tryFind (string y) s1.PortMaps.Orientation)
    |> function 
        |(Some(_), Some(_)) -> (s1,s2)
        | _ -> (s2, s1)


/// Function to determine if a point is within a Bounding Box.
/// It will return True if the point is within the box, False otherwise.
/// HLP 23: Author Gkamaletsos
let pointInBBox (point: XYPos) (bBox: BoundingBox): bool =
    let horizontally = point.X > bBox.TopLeft.X && point.X < (bBox.TopLeft.X + bBox.W)
    let vertically = point.Y > bBox.TopLeft.Y && point.Y < (bBox.TopLeft.Y + bBox.H)
    if (vertically=true) && (horizontally=true)
    then printfn "point in bBox detected"

    horizontally && vertically


/// Function to determine if and how a segment crosses a symbol from end to end.
/// This means that the edges of the segment are outside of the Symbol Bounding Box.
/// HLP 23: Author Gkamaletsos
let crossesBBox (startPos: XYPos) (endPos: XYPos) (bBox: BoundingBox): Orientation option =
    let horizontally = (((startPos.X < bBox.TopLeft.X) && (endPos.X > bBox.TopLeft.X + bBox.W)) || ((endPos.X < bBox.TopLeft.X) && (startPos.X > bBox.TopLeft.X + bBox.W))) && (startPos.Y > bBox.TopLeft.Y) && (startPos.Y < bBox.TopLeft.Y + bBox.H)
    let vertically = (((startPos.Y < bBox.TopLeft.Y) && (endPos.Y > bBox.TopLeft.Y + bBox.H)) || ((endPos.Y < bBox.TopLeft.Y) && (startPos.Y > bBox.TopLeft.Y + bBox.H))) && (startPos.X > bBox.TopLeft.X) && (startPos.X < bBox.TopLeft.X + bBox.W)
    if (vertically=true) || (horizontally=true)
    then printfn "crossing detected"

    if horizontally
    then Some Horizontal
    elif vertically
    then Some Vertical
    else None


/// Function to determine if a segment is intersecting a given Symbol in any way.
/// It returns an Orientation option. The intersection can be Horizontal, Vertical or None.
/// The function takes the whole wire and the index of the segment as input.
/// This is to accomodate the use of getAbsoluteSegmentPos from BusWireUpdateHelpers.
/// HLP 23: Author Gkamaletsos
let segOverSymbol (symbol: Symbol) (index: int) (wire: Wire): Orientation option =
    let startPos, endPos = getAbsoluteSegmentPos wire index
    let orientation = getSegmentOrientation startPos endPos
    let bBox = getSymbolBoundingBox symbol

    match pointInBBox startPos bBox || pointInBBox endPos bBox with
        | true  -> Some orientation
        | false -> crossesBBox startPos endPos bBox

/// Function that will determine if a symbol overlaps with any other symbols in the model.
/// It takes the symbol and the model as inputs. It also needs Sheet.boxesIntersect as input because this
/// could not be included in this module because of compilation order problems.
/// HLP 23: Author Gkamaletsos
let symbolOverlaps (symbol: Symbol) (model: SymbolT.Model) (boxesIntersect): bool =

    let tmpSymbolMap:Map<ComponentId, Symbol> = Map.remove symbol.Id model.Symbols

    (false, Map.toList tmpSymbolMap) ||>
    List.fold (fun state symbol2 ->
                    state || (boxesIntersect (getSymbolBoundingBox symbol) (getSymbolBoundingBox (snd(symbol2))))
                )


/// This function takes an oldPorts Map, an edge, an order list, and another list, 
/// and returns a sorted list of strings based on the given order list. If the 
/// string isn't in the order list, then it will be sorted at the end.
/// HLP 23: Author Parry
let sorted
    (oldPorts: Map<Edge,string list>) 
    (edge: Edge) (order: string list) 
    (other: string list) =
        oldPorts
        |> Map.find edge
        |> List.sortBy (fun port -> 
                List.findIndex ((=) port) order 
                |> function 
                   | -1 -> List.length other 
                   | i -> i)

/// This function takes two Maps of Edge to string lists, and returns a new Map of 
/// Edge to sorted string lists based on the optimal order of each edge.
/// HLP 23: Author Parry
let sortPorts 
    (oldPorts: Map<Edge,string list>) 
    (optimalOrder: Map<Edge,string list>) 
    : Map<Edge,string list> =
        optimalOrder 
        |> Map.fold (fun acc edge order -> 
                let other = 
                    oldPorts[edge]
                    |> List.distinct
                let sortedPorts = sorted oldPorts edge order other
                Map.add edge sortedPorts acc) 
            Map.empty
        
/// This function takes a Map of ConnectionIds to Wires and returns a list of tuples, 
/// where each tuple contains the names of the output and input ports of each wire.
/// HLP 23: Author Parry
let findPortIds (wires: Map<ConnectionId,Wire>) : ((string * string) List) = 
    let matching { OutputPort = outPort; InputPort = inPort } = string outPort, string inPort
    wires
    |> Map.toSeq
    |> Seq.map (fun (_, x) -> x)
    |> Seq.toList
    |> List.map matching

/// This function takes a list of edges and a corresponding list of ports needed, 
/// it then groups the string list into a list of all strings that are on a certain 
/// edge. This returns a Map of each edge to a list of ports connected to that edge. 
/// The returned Map is grouped based on the side of the edge (Top, Bottom, Left, or Right).
/// HLP 23: Author Parry
let groupByEdge (changingEdge: Edge list) (portsNeeded: string list) =
    let makeMapValue (inputList: (Edge*string)list) (edge:Edge) =
        let outputList = inputList |> List.map (fun (_,y) -> y)
        edge, (outputList |> List.rev)
    let splitList (originalList: (Edge*string)list) (edge:Edge)=
        originalList |> List.partition (fun (x,_) -> x = edge) |> (fun (x,_) -> makeMapValue x edge)
    let sortedList = (changingEdge,portsNeeded) ||> List.map2 (fun s1 s2 -> (s1,s2))
    Map.ofList [splitList sortedList Top ; splitList sortedList Bottom ; splitList sortedList Left ; splitList sortedList Right]

    
/// This function takes a Map of Edge to string lists and returns a list of strings of all ports.
/// HLP 23: Author Parry
let getListOfPortsFromMap (mapOfPorts:Map<Edge,string list>):string list =
    mapOfPorts
    |> Map.keys
    |> Seq.toList
    |> List.choose (fun edge ->
        match Map.tryFind edge mapOfPorts with
        | Some edges -> Some (edge, edges)
        | None -> None)
    |> List.sortBy (fun (edge, _) -> edge)
    |> List.collect snd

type XorY = X | Y

let getEdgePosition symbol edge = 
    match edge with
        | Left -> symbol.Pos.X
        | Right -> symbol.Pos.X + symbol.Component.W * Option.defaultValue 1. symbol.HScale
        | Top -> symbol.Pos.Y
        | Bottom -> symbol.Pos.Y - symbol.Component.H * Option.defaultValue 1. symbol.VScale

let getTotalLengthFromPortLabels portList = 
    let labelLengths = (
        List.map String.length portList
        |> List.map (float)
    )
    (0., labelLengths)
    ||> List.fold (fun acc e -> acc + e)
    |> (*) Constants.portTextCharWidth

let getMinimumHeightAndWidth symbol = 
    let portIdMap = getCustomPortIdMap symbol.Component
    let portMaps = makeMapsConsistent portIdMap symbol
    let convertIdsToLbls currMap edge idList =
        let lblLst = List.map (fun id -> portIdMap[id]) idList
        Map.add edge lblLst currMap
    let portLabels = 
        (Map.empty, portMaps.Order) ||> Map.fold convertIdsToLbls
    let minTopWidth = getTotalLengthFromPortLabels portLabels[Top]
    let minBottomWidth = getTotalLengthFromPortLabels portLabels[Bottom]
    let minLeftHeight = getTotalLengthFromPortLabels portLabels[Left]
    let minRightHeight = getTotalLengthFromPortLabels portLabels[Right]
    let minWidth = max minTopWidth minBottomWidth
    let minHeight = max minLeftHeight minRightHeight
    minHeight, minWidth


let formatSymbolPopup() : ReactElement =
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]
    div [] [
    bSpan "Custom components" ; tSpan " (found under "; bSpan "'THIS PROJECT'" ; tSpan ") can be awkwardly aligned with each other and other blocks."
    br []; br []
    ul [Style [ListStyle "disc"; MarginLeft "30px"]]
        [
            li [] [str "To use this function, select two symbols, the second symbol to be selected will be moved."]

            li [] [ str "The ports will then be reordered on both components so they all line up with each other."]
            
            li [] [str "The symbol will be resized to line up neatly with the first port so the sheet looks tidier."]
            
            li [] [str "Any wires that cross over the wires between these symbbols will be moved into the neatest ordering." ]
            ]
        ]

open PopupDrawingView

let testPopup : (BusWireT.Msg -> unit) -> PopupDialogData -> ReactElement =
    let body = div [] [str "test1"]
    let foot = div [] [str "test2"]
    closablePopupFunc "test" (fun _ -> body) (fun _ -> foot) []

type PopupChoice = {
    Number: int
    ButtonColor: IColor
    ButtonText: string
}

// let getEdgePosition e1 e2 (xy: XorY) wModel referenceSymbol symbolToResize = 
//     (Map.tryFind e1 referenceSymbol.PortMaps.Order, M)

let multipleChoicePopupFunc
        title 
        (body:(Msg->Unit)->ReactElement) 
        (choices: PopupChoice list)
        (buttonAction: int -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit) =
    let foot dispatch =
        let leftChildren =
           [div [] (choices
           |> List.mapi(fun i choice ->
                      div [] [Level.item [] [
                                  Button.button [
                                      Button.Color choice.ButtonColor
                                      Button.OnClick (buttonAction choice.Number dispatch)
                                  ] [ str choice.ButtonText ]
                              ];
                           br [] ]
                    )
                )]
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] leftChildren
            Level.right [] []
        ]

    closablePopupFunc title body foot []


//let multipleChoicePopupFuncTest
    //    title 
    //    (body:(Msg->Unit)->ReactElement) 
    //    (choices: PopupChoice list)
    //    (buttonAction: int -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit) =
    //let foot dispatch =
    //    let leftChildren =
    //       [div [] (choices
    //       //|> List.filter (fun c -> c.Number % 2 = 0)
    //       |> List.mapi(fun i choice ->
    //              match i % 2, i = List.length choices - 1 with
    //              | 1 , true ->
    //                   div [] []
    //              | 0, _ -> 
    //                  div [] [Level.item [] [
    //                              Button.button [
    //                                  Button.Color choice.ButtonColor
    //                                  Button.OnClick (buttonAction choice.Number dispatch)
    //                              ] [ str choice.ButtonText ]
    //                          ]
    //                         ]
                   
    //              | _  ->
    //                   div [] [ br [] ]
    //                )
    //            )]

    //    let rightChildren =
    //       [div [] (choices
    //       //|> List.filter (fun c -> c.Number % 2 = 0)
    //       |> List.tail
    //       |> List.mapi(fun i choice ->
    //              match i % 2 with
    //              | 0 -> 
    //                  div [] [Level.item [] [
    //                              Button.button [
    //                                  Button.Color choice.ButtonColor
    //                                  Button.OnClick (buttonAction choice.Number dispatch)
    //                              ] [ str choice.ButtonText ]
    //                          ]
    //                         ]
                           
    //              | _  ->
    //                   div [] [ br [] ]
    //                )
    //            )]
                
    //    Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
    //        Level.left [] leftChildren
    //        Level.right [] rightChildren
    //    ]

    //closablePopupFunc title body foot []


let resizeSelectPopup (symbol1: Symbol) (symbol2: Symbol) (edge1: Edge) (edge2: Edge)
    : ((BusWireT.Msg -> unit) -> PopupDialogData -> ReactElement) option =

    let body = div [] [str "This is complex resizing! Would you like to resize based on the inner ports or the outer ports?"]
    let (choices: PopupChoice list) =
        [{Number = 1; ButtonColor = IsPrimary; ButtonText = "Resize based on inner ports"};
        {Number = 2; ButtonColor = IsPrimary; ButtonText = "Resize based on outer ports"};
        {Number = 3; ButtonColor = IsLight; ButtonText = "Don't resize"};
        ]

    let buttonAction selectedChoiceNumber dispatch  _ =
        let symbolEdgePosDiff1 = abs (getEdgePosition symbol1 edge1 - getEdgePosition symbol2 edge2)
        let symbolEdgePosDiff2 = abs (getEdgePosition symbol1 edge2 - getEdgePosition symbol2 edge1)
        printfn "%A" symbolEdgePosDiff1
        printfn "%A" symbolEdgePosDiff2
        let newEdge1, newEdge2 = (
            if symbolEdgePosDiff1 < symbolEdgePosDiff2 then 
                match edge1 with
                    | Top | Bottom -> (edge1, edge2)
                    | Left | Right -> (edge2, edge1)
            else
                match edge1 with
                    | Top | Bottom -> (edge2, edge1)
                    | Left | Right -> (edge1, edge2)
        )
        printfn "edge1: %A, edge2: %A, newedge1: %A, newedge2: %A" edge1 edge2 newEdge1 newEdge2
        if selectedChoiceNumber = 1 then
            printfn "True, inner selected"
            dispatch <| ClosePopup
            dispatch <| BusWireT.SelectiveResize (symbol1, symbol2, newEdge1, newEdge2)
        elif selectedChoiceNumber = 2 then
            printfn "False, outer Selected"

            dispatch <| ClosePopup
            dispatch <| BusWireT.SelectiveResize (symbol1, symbol2, newEdge2, newEdge1)
        else
            dispatch <| ClosePopup


    multipleChoicePopupFunc
        "Select resize criterion" 
        (fun _ -> body)
        choices
        buttonAction 
    |> Some



let userGuidePopup() : ReactElement =
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]
    div [] [
    bSpan "For more help: ";        
        a [            
            Href "https://tomcl.github.io/issie/user-guide/";            
            Target "_blank";          
            Rel "noopener noreferrer"      
        ]
        [tSpan "https://tomcl.github.io/issie/user-guide/"]
    ]