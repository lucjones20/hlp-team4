module SmartSizeSymbol

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators
open SmartHelpers

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart resize symbol" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and one symbols in the BusWire model so could use the SmartHelper 
    function for the wires.
*)


// HLP23: AUTHOR Jones (entire file)

module Constants = 
    let gap = 0.7
    let wideGap = gap + 0.3



// type ConnectionType = LtR | RtL | TtB | BtT
/// type to characterise the different cases of resize we might encounter
type ResizeScenario = Mixed | LtR | RtL | TtB | BtT



let getPortEdges (wires: Map<ConnectionId, Wire>) (referenceSymbol: Symbol) (symbolToResize: Symbol) =
    wires
    |> Map.values
    |> Seq.toList
    |> List.map (fun x -> (x.OutputPort, x.InputPort))
    |> List.map (fun (op,ip) -> (
        match Map.tryFind (string op) referenceSymbol.PortMaps.Orientation with
            | Some(a) -> a
            | None -> Map.find (string op) symbolToResize.PortMaps.Orientation),(
        match Map.tryFind (string ip) symbolToResize.PortMaps.Orientation with
            | Some(a) -> a
            | None -> Map.find (string ip) referenceSymbol.PortMaps.Orientation
        ))
/// this function check is the checks whether or not it is possoble the wires connected in between two symbols 
/// to be parallel
/// this function will return true if it is possible and false if it is impossible
let isValidResize (wires: Map<ConnectionId, Wire>) (referenceSymbol: Symbol) (symbolToResize: Symbol): bool = 
    getPortEdges wires referenceSymbol symbolToResize
    |> Seq.toList
    |> List.map (fun (e1, e2) -> 
        match e1 with
            | Left -> e2 = Right
            | Right -> e2 = Left
            | Top -> e2 = Bottom
            | Bottom -> e2 = Top  
    )
    |> List.reduce(&&)


let getEdgeConnections (wires: Map<ConnectionId, Wire>) (referenceSymbol: Symbol) (symbolToResize: Symbol) =
    printfn "port edges %A" (getPortEdges wires referenceSymbol symbolToResize)
    ([], (getPortEdges wires referenceSymbol symbolToResize
    |> Seq.toList
    |> List.map (fun (e1, e2) ->
        match e1 with
            | Left -> if e2 = Right then Some(LtR) else None
            | Right -> if e2 = Left then Some(RtL) else None
            | Top -> if e2 = Bottom then Some(TtB) else None
            | Bottom -> if e2 = Top then Some(BtT) else None
    )))
    ||> List.fold(fun acc e-> if Option.isSome e then acc @ [(Option.get e)] else acc)


/// this function will figure out what scenario we are dealing with
/// the different scenarios are:
///     - the wires in between the symbols start and end horizontal (Horizontal)
///     - the wires in between the symbols start and end vertical (Vertical)
///     - the wires in between the symbols have diffrent orientations at the start and the end (Mixed)
/// this function also sorts the symbols into a reference symbol (whose height or width will not be changed)
/// and a symbol to resize
/// the reference symbol is chosen as the symbol that outputs into the other symbol (convention that I chose)
let getCase (wModel: BusWireT.Model) (s1: Symbol) (s2: Symbol): ResizeScenario * Symbol * Symbol = 
    let selectedWires = getSelectedSymbolWires wModel s1 s2
    if (Map.isEmpty selectedWires) then
        // if not (isValidResize (getSelectedSymbolWires wModel s2 s1) s2 s1) then 
        printfn "%A"(getEdgeConnections selectedWires s1 s2)
        match getEdgeConnections (getSelectedSymbolWires wModel s2 s1) s2 s1 with
            | [] -> Mixed, s1, s2
            | a::tail ->(match (List.fold (fun acc e -> 
                        match a with 
                            |LtR -> acc && (e = LtR || e = RtL)
                            |RtL -> acc && (e = LtR || e = RtL)
                            |TtB -> acc && (e = TtB || e = BtT)
                            |BtT -> acc && (e = TtB || e = BtT)
                            |_ -> false
                ) true tail) with
                        | true -> a, s2, s1
                        | false -> Mixed, s2, s1
                )
    //     else if hasHorizontalWires wModel s2 s1 then
    //         Horizontal, s2, s1
    //     else
    //         Vertical, s2, s1
    else
        printfn "this one %A"(getEdgeConnections selectedWires s1 s2)
        match getEdgeConnections (getSelectedSymbolWires wModel s1 s2) s2 s1 with
            | [] -> Mixed, s1, s2
            | a::tail ->(match (List.fold (fun acc e -> 
                        match a with 
                            |LtR -> acc && (e = LtR || e = RtL)
                            |RtL -> acc && (e = LtR || e = RtL)
                            |TtB -> acc && (e = TtB || e = BtT)
                            |BtT -> acc && (e = TtB || e = BtT)
                            |_ -> false
                ) true tail) with
                        | true -> a, s1, s2
                        | false -> Mixed, s1, s2
                )
        // else if hasHorizontalWires wModel s1 s2 then
        //     Horizontal, s1, s2
        // else 
        //     Vertical, s1, s2

let checkEdgeIsCorrect e1 e2 wModel referenceSymbol symbolToResize = 
    getSelectedSymbolWires wModel referenceSymbol symbolToResize
    |> Map.values
    |> Seq.toList
    |> List.map (fun x -> (x.OutputPort, x.InputPort))
    |> List.map (fun (x,y) -> (
        (List.contains (string x) (Map.find e1 referenceSymbol.PortMaps.Order)
        && 
        List.contains (string y) (Map.find e2 symbolToResize.PortMaps.Order))
        // ||
        // (List.contains (string x) (Map.find e2 referenceSymbol.PortMaps.Order)
        // && 
        // List.contains (string y) (Map.find e1 symbolToResize.PortMaps.Order))

    ))
    |> List.reduce (||)

let checkForConflictingWiring (xy:XorY) wModel referenceSymbol symbolToResize = 
    getSelectedSymbolWires wModel referenceSymbol symbolToResize
    |> Map.values
    |> Seq.toList
    |> List.map (fun x -> (x.InputPort, x.OutputPort))
    |> List.map (fun (x,y) -> Symbol.getTwoPortLocations wModel.Symbol x y)
    |> List.map (fun (ip,op) -> 
        match xy with
            |X -> abs (ip.X-op.X)
            |Y -> abs (ip.Y-op.Y))
    |> fun x -> printfn "%A" x ; x
    |> fun lst -> 
        match lst with
            | [] -> 0
            | [a] -> 1
            | a::tail -> match (List.fold (fun acc e -> acc && (e = a)) true tail) with
                            | false -> -1
                            | true -> List.length tail + 1


/// this function takes in 2 symbols: s1 (the reference symbol) and s2 (the symbol to resize)
/// the output is a tuple consisting of the Edge on which the input ports are located on s2 (the symbol to resize),
/// the list of output ports of s2 and the list of input ports of s2
let getOutputInputPorts (s1: Symbol) (s2: Symbol): Edge * Edge * string list * string list = 
    let outputEdge = (
        s1.Component.OutputPorts
        |> List.head
        |> fun x -> x.Id
        |> fun x -> Map.find x s1.PortMaps.Orientation
    ) 
    let inputEdge = (
        s2.Component.InputPorts
        |> List.head
        |> fun x -> x.Id
        |> fun x -> Map.find x s2.PortMaps.Orientation
    )
    (outputEdge, inputEdge, Map.find outputEdge s1.PortMaps.Order, Map.find inputEdge s2.PortMaps.Order)

/// this function will take in the output ports (from the reference symbol) and return an in order list
/// of the wires that connect the two symbols
/// the outputed list of wires will be needed to obtain the list of input ports (from the right symbol) that are
/// connected to the ouput ports. The list of input ports obtained will determine the new order in which 
/// the ports will be reordered on the right symbol 
let rec orderWiresByOutputPort selectedWires portList acc = 
    match portList with
        |a::tail -> (
            Map.filter (fun key value -> value.OutputPort = a) selectedWires
            |> Map.toList
            |> List.append acc
            |> orderWiresByOutputPort  selectedWires tail
            )
        |_ -> acc

/// type to help know on which axis we have to move the symbol

/// this function calculates the difference in either the X position or the Y position (depending on the input xy)
/// of the first wire's input and output ports 
/// the output of this function will be used to move the symbol so that the wires are parallel
let getPortOffset (xy: XorY) (selectedWires: Map<ConnectionId, Wire>) (symbolModel: SymbolT.Model): float=
    selectedWires
    |> Map.values
    |> Seq.map (fun (x: Wire) -> 
        match xy with 
            |Y -> ((getPortLocation None symbolModel (string x.OutputPort)).Y, (getPortLocation None symbolModel (string x.InputPort)).Y)
            |X -> ((getPortLocation None symbolModel (string x.OutputPort)).X, (getPortLocation None symbolModel (string x.InputPort)).X)
        )
    |> Seq.sort
    |> Seq.head
    |> fun (x,y) -> abs (y-x)

let getPortOffsetScale (xy: XorY) referenceSymbol symbolToResize refEdge resizeEdge symbolModel offset = 
    let refPort = List.head (Map.find refEdge referenceSymbol.PortMaps.Order)
    let resizePort = List.head (Map.find resizeEdge symbolToResize.PortMaps.Order)
    (refPort, resizePort)
    |> fun (x,y) -> (getPortLocation None symbolModel x, getPortLocation None symbolModel y)
    |> fun (port1, port2) -> 
        match xy with
            |X -> float (sign (port1.X - port2.X))
            |Y -> float (sign (port1.Y- port2.Y))
    |> (*) offset

/// this function reorders the input ports of symbolToResize to match the order of output ports
/// resulting in an ordering where wires don't cross eachother
let getNewPortOrder (outputPorts: string list) (inputPorts: string list) (symbolToResize: Symbol) (outputEdge: Edge) (selectedWires: Map<ConnectionId, Wire>) = 
    (List.map (fun x -> OutputPortId x) outputPorts, []) ||> orderWiresByOutputPort selectedWires
    |> List.map (fun (x, y) -> string (y.InputPort))
    |> List.rev 
    |> correctOrderingOfList inputPorts 
    |> fun x -> Map.add outputEdge x symbolToResize.PortMaps.Order


/// type used for the arguments of the updateSymbolSize function (code readability)
type UpdateSymbolSizeArgs = {
    heightOrWidthLens_ : Lens<Component, float> 
    xOrYLens_ : Lens<XYPos, float>
    XY: XorY 
    newDimension: float
    newPorts: Map<Edge, list<string>>
    referenceSymbol: Symbol
    symbolToResize: Symbol
}

/// this function will update the size of the symbol as well as align the symbol and its label box
/// so that either the top or the right are aligned
/// this function will also change the order of the ports
let updateSymbolSize (args: UpdateSymbolSizeArgs) =  
    let (refSymbolPos, refLabelPos) = (args.XY
    |> function
        |Y -> args.referenceSymbol.Pos.Y, args.referenceSymbol.LabelBoundingBox.TopLeft.Y
        |X -> args.referenceSymbol.Pos.X, args.referenceSymbol.LabelBoundingBox.TopLeft.X
    )
    Optic.set (component_ >-> args.heightOrWidthLens_) args.newDimension args.symbolToResize
    |> Optic.set (pos_ >-> args.xOrYLens_) refSymbolPos
    |> Optic.set (labelBoundingBox_ >-> topLeft_ >-> args.xOrYLens_) refLabelPos
    |> Optic.set (portMaps_ >-> order_) args.newPorts


/// this function changes the position of a symbol by the offset specified
let updateSymbolPosition (s: Symbol) (xOrYLens_: Lens<XYPos, float>) (offset: float): Symbol list = 
    s
    |> Optic.map (pos_ >-> xOrYLens_) (fun x -> x - offset)
    |> Optic.map (labelBoundingBox_ >-> topLeft_ >-> xOrYLens_) (fun x -> x - offset)
    |> fun x -> [x]

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from 
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace teh XML comment by something suitable concisely
/// stating what it does.
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
    (updateSymbolWires)
    (boxesIntercect)
     =

    getCase wModel symbolToSize otherSymbol
    |> fun (case, referenceSymbol, symbolToResize) -> 
        match case with 
            | RtL | LtR ->
                match checkForConflictingWiring X wModel referenceSymbol symbolToResize with
                    | -1 -> printfn "yessir"; {wModel with PopupViewFunc = resizeSelectPopup referenceSymbol symbolToResize Left Right} 
                    | 0 | 1 -> wModel
                    | _ -> 
                        let (edgePortSizeRefEdge, edgePortSizeResizeEdge) = (
                                match checkEdgeIsCorrect Left Right wModel referenceSymbol symbolToResize with
                                    | true -> Left, Right 
                                    | false -> Right, Left
                        )
                        let edgePortSizeRef = float (List.length (Map.find edgePortSizeRefEdge referenceSymbol.PortMaps.Order))
                        let edgePortSizeResize = float (List.length (Map.find edgePortSizeResizeEdge symbolToResize.PortMaps.Order))
                        let ratio = (((edgePortSizeResize - 1.0) + 2.0 * Constants.gap) / ((edgePortSizeRef - 1.0) + 2.0 * Constants.gap))
                        let newVerticalScale = (Option.defaultValue 1. referenceSymbol.VScale) *  referenceSymbol.Component.H * (ratio / symbolToResize.Component.H)
                        // get the relevant input and output ports of the symbols
                        let (outputEdge, inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToResize

                        let newPorts = (
                            //getSelectedSymbolWires wModel referenceSymbol symbolToResize
                            //|> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge 
                            symbolToResize.PortMaps.Order
                        ) 
                        
                        let rightSymbol' = (
                            Optic.set (vScale_) (Some(newVerticalScale)) symbolToResize
                            |> Optic.set (pos_ >-> y_) referenceSymbol.Pos.Y
                            |> Optic.set (labelBoundingBox_ >-> topLeft_ >-> y_ ) referenceSymbol.LabelBoundingBox.TopLeft.Y
                            |> Optic.set (portMaps_ >-> order_) newPorts
                        )
                        
                        // update the model with the new symbol
                        let interModel = updateModelSymbols wModel [rightSymbol']
                        if symbolOverlaps rightSymbol' interModel.Symbol boxesIntercect
                        then printfn "Overlap Detected"
                        else printfn "no overlap"
                        

                        // currently the model would move the symbols so that the top of the symbols are on the same line 
                        // the wires would be parallel if the ports are aligned, but they are not necessarely aligned at this point
                        // the next part of the code makes sure that the wires are aligned

                        // slide the position of the symbol by the offset calculated
                        ((getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
                        ||> getPortOffset Y
                        |> getPortOffsetScale Y referenceSymbol symbolToResize edgePortSizeRefEdge edgePortSizeResizeEdge interModel.Symbol
                        |> updateSymbolPosition rightSymbol' y_
                        |> updateModelSymbols interModel
                        |> fun x -> updateSymbolWires x rightSymbol'.Id)
                    // |true -> printfn "yessir"; {wModel with PopupViewFunc = Some(testPopup)}
            | TtB | BtT -> 
                match checkForConflictingWiring Y wModel referenceSymbol symbolToResize with
                    | -1 -> printfn "yessir"; {wModel with PopupViewFunc = resizeSelectPopup referenceSymbol symbolToResize Top Bottom} 
                    | 0 | 1 -> wModel
                    | _ -> 
                        // get the relevant input and output ports of the symbols
                        let (outputEdge, inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToResize //AHHHHHHHHHH
                        // compute the new width of the symbol to resize
                        // this will result in all the wires between both signals being parallel if the symbols are aligned correctly
                        let newWidth = referenceSymbol.Component.W * (((float (List.length (Map.find inputEdge (symbolToResize.PortMaps.Order))) - 1.0) + 2.0 * Constants.wideGap) / ((float (List.length (Map.find outputEdge referenceSymbol.PortMaps.Order)) - 1.0 ) + 2.0 * Constants.wideGap))
                        let (edgePortSizeRefEdge, edgePortSizeResizeEdge) = (
                            match checkEdgeIsCorrect Top Bottom wModel referenceSymbol symbolToResize with
                                | true -> Top, Bottom 
                                | false -> Bottom, Top
                        )

                        let edgePortSizeRef = float (List.length (Map.find edgePortSizeRefEdge referenceSymbol.PortMaps.Order))
                        let edgePortSizeResize = float (List.length (Map.find edgePortSizeResizeEdge symbolToResize.PortMaps.Order))
                        let ratio = (((edgePortSizeResize - 1.0) + 2.0 * Constants.wideGap) / ((edgePortSizeRef - 1.0) + 2.0 * Constants.wideGap))
                        let newHorizontalScale = (Option.defaultValue 1. referenceSymbol.HScale) * referenceSymbol.Component.W * (ratio / symbolToResize.Component.W)
                        
                        let newPorts = (
                            //getSelectedSymbolWires wModel referenceSymbol symbolToResize
                            //|> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge
                            symbolToResize.PortMaps.Order
                        )
                        let rightSymbol' =(
                            Optic.set (hScale_) (Some(newHorizontalScale)) symbolToResize
                                |> Optic.set (pos_ >-> x_) referenceSymbol.Pos.X
                                |> Optic.set (labelBoundingBox_ >-> topLeft_ >-> x_ ) referenceSymbol.LabelBoundingBox.TopLeft.X
                                |> Optic.set (portMaps_ >-> order_) newPorts
                        )
                        // update the model with the new symbols
                        let interModel = updateModelSymbols wModel [rightSymbol']

                        // slide the position of the symbol by the calculated offset
                        ((getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
                        ||> getPortOffset X
                        |> getPortOffsetScale X referenceSymbol symbolToResize edgePortSizeRefEdge edgePortSizeResizeEdge interModel.Symbol
                        |> updateSymbolPosition rightSymbol' x_
                        |> updateModelSymbols interModel
                        |> fun x -> updateSymbolWires x rightSymbol'.Id)



            | _ -> wModel
        
 



    // |> function
    //     | Horizontal, referenceSymbol, symbolToResize ->
    //         printfn "detected horizontal";
    //         // compute the new height of the symbol to resize
    //         // this will result in all the wires between both signals being parallel if the symbols are aligned correctly
    //         let newHeight = referenceSymbol.Component.H * (((float symbolToResize.Component.InputPorts.Length - 1.0) + 2.0 * Constants.gap) / ((float referenceSymbol.Component.OutputPorts.Length - 1.0 ) + 2.0 * Constants.gap))

    //         // get the relevant input and output ports of the symbols
    //         let (outputEdge, inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToResize

    //         let newPorts = (
    //             getSelectedSymbolWires wModel referenceSymbol symbolToResize
    //             |> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge 
    //         ) 
            
    //         // arguments for updateSymbolSize
    //         let args = {
    //             heightOrWidthLens_ = h_;
    //             xOrYLens_ = y_;
    //             XY = Y;
    //             newDimension = newHeight;
    //             newPorts = newPorts;
    //             referenceSymbol = referenceSymbol;
    //             symbolToResize = symbolToResize
    //         }
    //         let rightSymbol' = updateSymbolSize args
            
    //         // update the model with the new symbol
    //         let interModel = updateModelSymbols wModel [rightSymbol']
            

    //         // currently the model would move the symbols so that the top of the symbols are on the same line 
    //         // the wires would be parallel if the ports are aligned, but they are not necessarely aligned at this point
    //         // the next part of the code makes sure that the wires are aligned

    //         // slide the position of the symbol by the offset calculated
    //         (getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
    //         ||> getPortOffset Y
    //         |> updateSymbolPosition rightSymbol' y_
    //         |> updateModelSymbols interModel
    //         |> fun x -> updateSymbolWires x rightSymbol'.Id 

    
    //     | Vertical, referenceSymbol, symbolToResize -> 
    //         printfn "detected vertical"
            
    //         printfn "%A" referenceSymbol.Component.W
    //         printfn "%A" symbolToResize.PortMaps.Order
    //         // get the relevant input and output ports of the symbols
    //         let (outputEdge, inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToResize //AHHHHHHHHHH
    //         // compute the new width of the symbol to resize
    //         // this will result in all the wires between both signals being parallel if the symbols are aligned correctly
    //         let newWidth = referenceSymbol.Component.W * (((float (List.length (Map.find inputEdge (symbolToResize.PortMaps.Order))) - 1.0) + 2.0 * Constants.wideGap) / ((float (List.length (Map.find outputEdge referenceSymbol.PortMaps.Order)) - 1.0 ) + 2.0 * Constants.wideGap))
            
    //         let newPorts = (
    //             getSelectedSymbolWires wModel referenceSymbol symbolToResize
    //             |> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge
    //         )

    //         // arguments for updateSymbolSize
    //         let args = {
    //             heightOrWidthLens_ = w_;
    //             xOrYLens_ = x_;
    //             XY = Y;
    //             newDimension = newWidth;
    //             newPorts = newPorts;
    //             referenceSymbol = referenceSymbol;
    //             symbolToResize = symbolToResize
    //         }
    //         let rightSymbol' = updateSymbolSize args

    //         // update the model with the new symbols
    //         let interModel = updateModelSymbols wModel [rightSymbol']

    //         // slide the position of the symbol by the calculated offset
    //         (getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
    //         ||> getPortOffset X
    //         |> updateSymbolPosition rightSymbol' x_
    //         |> updateModelSymbols interModel
    //         |> fun x -> updateSymbolWires x rightSymbol'.Id

    //     | Mixed, _ , _ -> 
    //         printfn "detected mixed"

    //         // symbols don't allow parallel wires
    //         // do nothing and return the model
    //         wModel
    //     | RtL, referenceSymbol, symbolToResize ->

    //         let currentHorizontalScale = Option.defaultValue 1. referenceSymbol.HScale 
    //         let currentVerticalScale = Option.defaultValue 1. referenceSymbol.VScale

    //         let edgePortSizeRef = float (List.length (Map.find Right referenceSymbol.PortMaps.Order))
    //         let edgePortSizeResize = float (List.length (Map.find Left symbolToResize.PortMaps.Order))
    //         let ratio = (((edgePortSizeResize - 1.0) + 2.0 * Constants.gap) / ((edgePortSizeRef - 1.0) + 2.0 * Constants.gap))
    //         let newVerticalScale = referenceSymbol.Component.H * (ratio / symbolToResize.Component.H)
    //         // let newHeightScale = referenceSymbol.Component.H
    //         // get the relevant input and output ports of the symbols
    //         let (outputEdge, inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToResize

    //         let newPorts = (
    //             getSelectedSymbolWires wModel referenceSymbol symbolToResize
    //             |> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge 
    //         ) 
            
    //         // arguments for updateSymbolSize
    //         // let args = {
    //         //     heightOrWidthLens_ = h_;
    //         //     xOrYLens_ = y_;
    //         //     XY = Y;
    //         //     newDimension = newHeight;
    //         //     newPorts = newPorts;
    //         //     referenceSymbol = referenceSymbol;
    //         //     symbolToResize = symbolToResize
    //         // }
    //         // let rightSymbol' = updateSymbolSize args
    //         // let (refSymbolPos, refLabelPos) = (args.XY
    //         //     |> function
    //         //         |Y -> args.referenceSymbol.Pos.Y, args.referenceSymbol.LabelBoundingBox.TopLeft.Y
    //         //         |X -> args.referenceSymbol.Pos.X, args.referenceSymbol.LabelBoundingBox.TopLeft.X
    //         //     )
    //         let rightSymbol' = (
    //             Optic.set (vScale_) (Some(newVerticalScale)) symbolToResize
    //             |> Optic.set (pos_ >-> y_) referenceSymbol.Pos.Y
    //             |> Optic.set (labelBoundingBox_ >-> topLeft_ >-> y_ ) referenceSymbol.LabelBoundingBox.TopLeft.Y
    //             |> Optic.set (portMaps_ >-> order_) newPorts
    //         )
            
    //         // update the model with the new symbol
    //         let interModel = updateModelSymbols wModel [rightSymbol']
            

    //         // currently the model would move the symbols so that the top of the symbols are on the same line 
    //         // the wires would be parallel if the ports are aligned, but they are not necessarely aligned at this point
    //         // the next part of the code makes sure that the wires are aligned

    //         // slide the position of the symbol by the offset calculated
    //         (getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
    //         ||> getPortOffset Y
    //         |> updateSymbolPosition rightSymbol' y_
    //         |> updateModelSymbols interModel
    //         |> fun x -> updateSymbolWires x rightSymbol'.Id 
        
    //     |LtR, referenceSymbol, symbolToResize ->

    //         let newHeight= referenceSymbol.Component.H * (((float symbolToResize.Component.InputPorts.Length - 1.0) + 2.0 * Constants.gap) / ((float referenceSymbol.Component.OutputPorts.Length - 1.0 ) + 2.0 * Constants.gap))

    //         let currentHorizontalScale = Option.defaultValue 1. referenceSymbol.HScale 
    //         let currentVerticalScale = Option.defaultValue 1. referenceSymbol.VScale

    //         let edgePortSizeRef = float (List.length (Map.find Left referenceSymbol.PortMaps.Order))
    //         let edgePortSizeResize = float (List.length (Map.find Right symbolToResize.PortMaps.Order))
    //         printfn "%A" edgePortSizeRef
    //         printfn "%A" edgePortSizeResize
    //         let ratio = (((edgePortSizeResize - 1.0) + 2.0 * Constants.gap) / ((edgePortSizeRef - 1.0) + 2.0 * Constants.gap))
    //         let newVerticalScale = referenceSymbol.Component.H * (ratio / symbolToResize.Component.H)
    //         // let newHeightScale = referenceSymbol.Component.H
    //         // get the relevant input and output ports of the symbols
    //         let (outputEdge, inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToResize

    //         let newPorts = (
    //             getSelectedSymbolWires wModel referenceSymbol symbolToResize
    //             |> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge 
    //         ) 
            
    //         // arguments for updateSymbolSize
    //         // let args = {
    //         //     heightOrWidthLens_ = h_;
    //         //     xOrYLens_ = y_;
    //         //     XY = Y;
    //         //     newDimension = newHeight;
    //         //     newPorts = newPorts;
    //         //     referenceSymbol = referenceSymbol;
    //         //     symbolToResize = symbolToResize
    //         // }
    //         // let rightSymbol' = updateSymbolSize args
    //         // let (refSymbolPos, refLabelPos) = (args.XY
    //         //     |> function
    //         //         |Y -> args.referenceSymbol.Pos.Y, args.referenceSymbol.LabelBoundingBox.TopLeft.Y
    //         //         |X -> args.referenceSymbol.Pos.X, args.referenceSymbol.LabelBoundingBox.TopLeft.X
    //         //     )
    //         let rightSymbol' = (
    //             Optic.set (vScale_) (Some(newVerticalScale)) symbolToResize
    //             |> Optic.set (pos_ >-> y_) referenceSymbol.Pos.Y
    //             |> Optic.set (labelBoundingBox_ >-> topLeft_ >-> y_ ) referenceSymbol.LabelBoundingBox.TopLeft.Y
    //             |> Optic.set (portMaps_ >-> order_) newPorts
    //         )
            
    //         // update the model with the new symbol
    //         let interModel = updateModelSymbols wModel [rightSymbol']
            

    //         // currently the model would move the symbols so that the top of the symbols are on the same line 
    //         // the wires would be parallel if the ports are aligned, but they are not necessarely aligned at this point
    //         // the next part of the code makes sure that the wires are aligned

    //         // slide the position of the symbol by the offset calculated
    //         (getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
    //         ||> getPortOffset Y
    //         |> updateSymbolPosition rightSymbol' y_
    //         |> updateModelSymbols interModel
    //         |> fun x -> updateSymbolWires x rightSymbol'.Id 

let selectiveResizeSymbol 
    (wModel: BusWireT.Model)
    (referenceSymbol: Symbol)
    (symbolToResize: Symbol)
    (edge1: Edge)
    (edge2: Edge)
    updateSymbolWires
    =
    getCase wModel referenceSymbol symbolToResize
    |> fun (case, referenceSymbol, symbolToResize) ->
        match case with
            | RtL | LtR -> 
                let (edgePortSizeRefEdge, edgePortSizeResizeEdge) = (
                    edge1, edge2
                )
                let edgePortSizeRef = float (List.length (Map.find edgePortSizeRefEdge referenceSymbol.PortMaps.Order))
                let edgePortSizeResize = float (List.length (Map.find edgePortSizeResizeEdge symbolToResize.PortMaps.Order))
                let ratio = (((edgePortSizeResize - 1.0) + 2.0 * Constants.gap) / ((edgePortSizeRef - 1.0) + 2.0 * Constants.gap))
                let newVerticalScale = (Option.defaultValue 1. referenceSymbol.VScale) * referenceSymbol.Component.H * (ratio / symbolToResize.Component.H)
                let rightSymbol' = (
                    Optic.set (vScale_) (Some(newVerticalScale)) symbolToResize
                    |> Optic.set (pos_ >-> y_) referenceSymbol.Pos.Y
                    |> Optic.set (labelBoundingBox_ >-> topLeft_ >-> y_ ) referenceSymbol.LabelBoundingBox.TopLeft.Y
                    // |> Optic.set (portMaps_ >-> order_) symbolToResize.PortMaps.
                )
                let interModel = updateModelSymbols wModel [rightSymbol']
                printfn "%A" newVerticalScale
                ((getSelectedEdgeWires wModel referenceSymbol symbolToResize edgePortSizeRefEdge edgePortSizeResizeEdge, interModel.Symbol)
                ||> getPortOffset Y
                |> updateSymbolPosition rightSymbol' y_
                |> updateModelSymbols interModel
                |> fun x -> updateSymbolWires x rightSymbol'.Id)
            | TtB | BtT -> 
                let (edgePortSizeRefEdge, edgePortSizeResizeEdge) = (
                    edge1, edge2
                )
                let edgePortSizeRef = float (List.length (Map.find edgePortSizeRefEdge referenceSymbol.PortMaps.Order))
                let edgePortSizeResize = float (List.length (Map.find edgePortSizeResizeEdge symbolToResize.PortMaps.Order))
                let ratio = (((edgePortSizeResize - 1.0) + 2.0 * Constants.wideGap) / ((edgePortSizeRef - 1.0) + 2.0 * Constants.wideGap))
                let newHorizontalScale = (Option.defaultValue 1. referenceSymbol.HScale) * referenceSymbol.Component.W * (ratio / symbolToResize.Component.W)

                let rightSymbol' = (
                    Optic.set (hScale_) (Some(newHorizontalScale)) symbolToResize
                    |> Optic.set (pos_ >-> x_) referenceSymbol.Pos.X
                    |> Optic.set (labelBoundingBox_ >-> topLeft_ >-> x_ ) referenceSymbol.LabelBoundingBox.TopLeft.X
                    // |> Optic.set (portMaps_ >-> order_) symbolToResize.PortMaps.
                )
                let interModel = updateModelSymbols wModel [rightSymbol']
                ((getSelectedEdgeWires wModel referenceSymbol symbolToResize edgePortSizeRefEdge edgePortSizeResizeEdge, interModel.Symbol)
                ||> getPortOffset X
                |> updateSymbolPosition rightSymbol' x_
                |> updateModelSymbols interModel
                |> fun x -> updateSymbolWires x rightSymbol'.Id)
            |Mixed -> failwith "should never happen"

