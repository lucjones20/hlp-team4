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



/// type to characterise the different cases of resize we might encounter
type ResizeScenario = |Horizontal | Vertical | Mixed


/// this function check is the checks whether or not it is possoble the wires connected in between two symbols 
/// to be parallel
/// this function will return true if it is possible and false if it is impossible
let isValidResize (wires: Map<ConnectionId, Wire>) (referenceSymbol: Symbol) (symbolToResize: Symbol): bool = 
    wires
    |> Map.values
    |> Seq.map (fun x -> (x.OutputPort, x.InputPort))
    |> Seq.map (fun (op,ip) -> (Map.find (string op) referenceSymbol.PortMaps.Orientation), (Map.find (string ip) symbolToResize.PortMaps.Orientation))
    |> Seq.map (fun (e1, e2) -> 
        match e1 with
            | Left -> e2 = Right
            | Right -> e2 = Left
            | Top -> e2 = Bottom
            | Bottom -> e2 = Top  
    )
    |> Seq.reduce(&&)


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
        if not (isValidResize (getSelectedSymbolWires wModel s2 s1) s2 s1) then 
            Mixed, s2, s1
        else if hasHorizontalWires wModel s2 s1 then
            Horizontal, s2, s1
        else
            Vertical, s2, s1
    else
        if not (isValidResize selectedWires s1 s2) then
            Mixed, s1, s2
        else if hasHorizontalWires wModel s1 s2 then
            Horizontal, s1, s2
        else 
            Vertical, s1, s2

/// this function takes in 2 symbols: s1 (the reference symbol) and s2 (the symbol to resize)
/// the output is a tuple consisting of the Edge on which the input ports are located on s2 (the symbol to resize),
/// the list of output ports of s2 and the list of input ports of s2
let getOutputInputPorts (s1: Symbol) (s2: Symbol): Edge * string list * string list = 
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
    (inputEdge, Map.find outputEdge s1.PortMaps.Order, Map.find inputEdge s2.PortMaps.Order)

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
type XY = X | Y

/// this function calculates the difference in either the X position or the Y position (depending on the input xy)
/// of the first wire's input and output ports 
/// the output of this function will be used to move the symbol so that the wires are parallel
let getPortOffset (xy: XY) (selectedWires: Map<ConnectionId, Wire>) (symbolModel: SymbolT.Model): float=
    selectedWires
    |> Map.values
    |> Seq.map (fun (x: Wire) -> 
        match xy with 
            |Y -> ((getPortLocation None symbolModel (string x.OutputPort)).Y, (getPortLocation None symbolModel (string x.InputPort)).Y)
            |X -> ((getPortLocation None symbolModel (string x.OutputPort)).X, (getPortLocation None symbolModel (string x.InputPort)).X)
        )
    |> Seq.sort
    |> Seq.head
    |> fun (x,y) -> y-x

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
    XY: XY 
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
        : BusWireT.Model =

    getCase wModel symbolToSize otherSymbol
    |> function
        | Horizontal, referenceSymbol, symbolToResize ->

            // compute the new height of the symbol to resize
            // this will result in all the wires between both signals being parallel if the symbols are aligned correctly
            let newHeight = referenceSymbol.Component.H * (((float symbolToResize.Component.InputPorts.Length - 1.0) + 2.0 * Constants.gap) / ((float referenceSymbol.Component.OutputPorts.Length - 1.0 ) + 2.0 * Constants.gap))

            // get the relevant input and output ports of the symbols
            let (inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToSize

            let newPorts = (
                getSelectedSymbolWires wModel referenceSymbol symbolToResize
                |> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge 
            ) 
            
            // arguments for updateSymbolSize
            let args = {
                heightOrWidthLens_ = h_;
                xOrYLens_ = y_;
                XY = Y;
                newDimension = newHeight;
                newPorts = newPorts;
                referenceSymbol = referenceSymbol;
                symbolToResize = symbolToResize
            }
            let rightSymbol' = updateSymbolSize args
            
            // update the model with the new symbol
            let interModel = updateModelSymbols wModel [rightSymbol']
            

            // currently the model would move the symbols so that the top of the symbols are on the same line 
            // the wires would be parallel if the ports are aligned, but they are not necessarely aligned at this point
            // the next part of the code makes sure that the wires are aligned

            // slide the position of the symbol by the offset calculated
            (getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
            ||> getPortOffset Y
            |> updateSymbolPosition rightSymbol' y_
            |> updateModelSymbols interModel
            |> fun x -> updateSymbolWires x rightSymbol'.Id 

    
        | Vertical, referenceSymbol, symbolToResize -> 
            printfn "detected vertical"
            
            // compute the new width of the symbol to resize
            // this will result in all the wires between both signals being parallel if the symbols are aligned correctly
            let newWidth = referenceSymbol.Component.W * (((float symbolToResize.Component.InputPorts.Length - 1.0) + 2.0 * Constants.wideGap) / ((float referenceSymbol.Component.OutputPorts.Length - 1.0 ) + 2.0 * Constants.wideGap))

            // get the relevant input and output ports of the symbols
            let (inputEdge, outputPorts, inputPorts) = getOutputInputPorts referenceSymbol symbolToResize
            
            let newPorts = (
                getSelectedSymbolWires wModel referenceSymbol symbolToResize
                |> getNewPortOrder outputPorts inputPorts symbolToResize inputEdge
            )

            // arguments for updateSymbolSize
            let args = {
                heightOrWidthLens_ = w_;
                xOrYLens_ = x_;
                XY = Y;
                newDimension = newWidth;
                newPorts = newPorts;
                referenceSymbol = referenceSymbol;
                symbolToResize = symbolToResize
            }
            let rightSymbol' = updateSymbolSize args

            // update the model with the new symbols
            let interModel = updateModelSymbols wModel [rightSymbol']

            // slide the position of the symbol by the calculated offset
            (getSelectedSymbolWires interModel referenceSymbol rightSymbol', interModel.Symbol)
            ||> getPortOffset X
            |> updateSymbolPosition rightSymbol' x_
            |> updateModelSymbols interModel
            |> fun x -> updateSymbolWires x rightSymbol'.Id

        | Mixed, _ , _ -> 
            printfn "detected mixed"
            // symbols don't allow parallel wires
            // do nothing and return the model
            wModel
