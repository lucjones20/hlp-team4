module SmartHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

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



/// Find the Y position of input ports for a given symbol
/// Function takes in a SymbolT model and a symbol
/// This uses the getPortLocation from the Symbol module and applies it to the
/// list of port ids located on the left side of the module
/// HLP23: AUTHOR Jones
let findInputPortYPos (model: SymbolT.Model) (symbol: Symbol): XYPos list = 
    symbol.PortMaps.Order.TryFind Left // try to get list of port ids of the symbol
    |> Option.defaultValue [] // extract the list from option
    |> List.map (Symbol.getPortLocation None model) // map getPortLocation onto the list of port ids


/// Find the Y position of output ports of a given symbol
/// Function takes in a SymbolT model and a symbol
/// This uses the getPortLocation from the Symbol module and applies it to the
/// list of port ids located on the right side of the module
/// HLP23: AUTHOR Jones
let findOutputPortYPos (model: SymbolT.Model) (symbol: Symbol): XYPos list= 
    symbol.PortMaps.Order.TryFind Right // try to get list of port ids of the symbol
    |> Option.defaultValue [] // extract list from option
    |> List.map (Symbol.getPortLocation None model) // map getPortLocation onto list of port ids

/// sort 2 symbols with respect to their X position
/// the left most symbol will be the first element in the return tuple
/// HLP23: AUTHOR Jones
let sortSymbolsLeftToRight (s1: Symbol) (s2: Symbol): Symbol * Symbol= 
    if (toX s1.Pos < toX s2.Pos) then s1, s2
    else s2, s1

/// returns a map of the wires connected from s1 to s2
/// HLP23: AUTHOR Jones
let getSelectedSymbolWires (wModel: BusWireT.Model) (s1: Symbol) (s2: Symbol): Map<ConnectionId, Wire> = 
    let matchInputOutputPorts key value : bool= 
        ((s1.Component.OutputPorts
        |> List.map (fun (x:Port) -> x.Id)
        |> List.contains (string value.OutputPort)) // check that one of the left symbol's output ports is the wire's output port
        && ( s2.Component.InputPorts
        |> List.map (fun (x:Port) -> x.Id)
        |> List.contains (string value.InputPort))) // check that one of the right symbol's input ports is the wire's input port
    wModel.Wires
    |> Map.filter matchInputOutputPorts


/// lenses used to edit symbols 
/// HLP23: should be placed in DrawModelType and CommonTypes
/// HLP23: AUTHOR Jones
let pos_: Lens<Symbol,XYPos> = Lens.create (fun a -> a.Pos) (fun s a -> {a with Pos = s}) // change Pos of Symbol

let x_: Lens<XYPos,float>= Lens.create (fun a -> a.X) (fun s a -> {a with X = s}) // change Y of XYPos

let y_: Lens<XYPos,float> = Lens.create (fun a -> a.Y) (fun s a -> {a with Y = s}) // change X of XYPos

let labelBoundingBox_: Lens<Symbol, BoundingBox> = // change LabelBoundingBox of Symbol
    Lens.create (fun a -> a.LabelBoundingBox) (fun s a -> {a with LabelBoundingBox = s}) 

let topLeft_: Lens<BoundingBox, XYPos> = Lens.create (fun a -> a.TopLeft) (fun s a -> {a with TopLeft = s}) // change TopLeft of LabelBoundingBox

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