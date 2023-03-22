module SmartPortOrder
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

/// HLP 23: Author Parry

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)

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
        //||
        //((s2.Component.OutputPorts
        //|> List.map (fun (x:Port) -> x.Id)
        //|> List.contains (string value.OutputPort)) // check that one of the left symbol's output ports is the wire's output port
        //&& ( s1.Component.InputPorts
        //|> List.map (fun (x:Port) -> x.Id)
        //|> List.contains (string value.InputPort)))
        ////  //check that one of the right symbol's input ports is the wire's input port
    wModel.Wires
    |> Map.filter matchInputOutputPorts

/// This function takes two Maps of Edge to string lists, and returns a new Map of Edge to string 
/// lists where each list is ordered according to the correct order from the correctOrderList.
/// Makes use of Author Jones' correctOrderingOfList helper function
/// HLP 23: Author Parry
let correctOrderingOfPorts 
    (originalList: Map<Edge, string list>) 
    (correctOrderList: Map<Edge, string list>) 
    : Map<Edge, string list> =
        originalList
        |> Map.fold (fun acc edge originalPorts -> 
            let correctPorts = Map.find edge correctOrderList
            let orderedPorts = SmartHelpers.correctOrderingOfList originalPorts correctPorts
            Map.add edge orderedPorts acc) Map.empty

/// This function takes a list of tuples, where each tuple contains two strings, and a reference 
/// list, and returns a list of strings sorted according to the order of the reference list.
/// HLP 23: Author Parry
let sortTupleListByNewList (tupleList: List<string*string>) (refList:string list) : string list =
    let refIndex = Map.ofList (List.mapi (fun i x -> (x, i)) refList)
    let sortByRefIndex ((x, y): string * string) =
        match refIndex.TryGetValue x with
        | true, index -> index, y
        | _ -> failwith "Element not found in reference list"
    tupleList |> List.sortBy sortByRefIndex |> List.map snd

/// This function takes a list of tuples, where each tuple contains two strings, and a reference list, 
/// and returns a list of strings sorted according to the order of the second string in the tuple.
/// HLP 23: Author Parry
let sortTupleListByList (tupleList: List<string*string>) (refList:string list) : string list =
    let swapList = tupleList |> List.map (fun (x,y) -> y,x)
    (sortTupleListByNewList swapList refList)

/// This function takes a list of edges, a reference list, and a list of tuples, where each tuple 
/// contains a string and an edge. It returns a sorted list of edges according to the order of 
/// the string in the tuple.
/// HLP 23: Author Parry
let sortEdgeByList 
    (orderEdge: Edge list) 
    (refList:string list) 
    (tupleList: List<string*string>) 
    : Edge list =
        let refIndex = Map.ofList (List.mapi (fun i x -> (x, i)) refList)
        let sortByRefIndex ((x, y): string * Edge) =
            match refIndex.TryGetValue x with
            | true, index -> index, y
            | _ -> failwith "Element not found in reference list"
        ((tupleList |> List.map snd), orderEdge) ||> List.map2 (fun x y -> x,y)
        |> List.sortBy sortByRefIndex |> List.map snd

/// To test this, it must be given two symbols interconnected by wires. It then reorders the ports on
/// symbolToOrder so that the connecting wires do not cross.
/// Tt should work out the interconnecting wires (wiresToOrder) from 
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).
let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
    (updateSymbolWires)
        : BusWireT.Model =
    let sModel = wModel.Symbol
    let connectingOutputPortsIds = SmartHelpers.findPortIds( getSelectedSymbolWires wModel symbolToOrder otherSymbol)
    let connectingInputPortsIds = 
        SmartHelpers.findPortIds (getSelectedSymbolWires wModel otherSymbol symbolToOrder)
        |> List.map (fun (x,y) -> y,x)
    let connectingPortsIds = connectingOutputPortsIds @ connectingInputPortsIds
    
    
    let portsToGetOrderFrom = connectingPortsIds |> List.map (fun (_,x) -> x)

    // These are so the ports can be easily regrouped based on the edge they are on
    let orderOfMovingEdges = 
        connectingPortsIds 
        |> List.map (fun (x,_) -> x) 
        |> List.map symbolToOrder.PortMaps.Orientation.TryFind
        |> List.choose id
    let orderOfstaticEdges = 
        portsToGetOrderFrom
        |> List.map otherSymbol.PortMaps.Orientation.TryFind
        |> List.choose id
        
    let mapOfStaticPortsByEdge = SmartHelpers.groupByEdge orderOfstaticEdges portsToGetOrderFrom

    // The optimal order is so the ports line up with the corresponding symbol as this would always uncross all wires
    let optimalOrder = otherSymbol.PortMaps.Order
    let oldOrder = symbolToOrder.PortMaps.Order

    // Sorts non changing ports that are connected so the optimal order for the connected ports is found
    let sortedMapOfStaticPorts = SmartHelpers.sortPorts mapOfStaticPortsByEdge optimalOrder 
    // list in order required for the static ports - makes it easier to sort the connectingPortsIds
    let sortedListOfStaticPorts = SmartHelpers.getListOfPortsFromMap sortedMapOfStaticPorts 
    
    let sortedConnectingPorts = sortTupleListByList connectingPortsIds sortedListOfStaticPorts
    let sortedOrderofMovingEdges = sortEdgeByList orderOfMovingEdges sortedListOfStaticPorts connectingPortsIds
    
    let changedOrder = SmartHelpers.groupByEdge sortedOrderofMovingEdges sortedConnectingPorts

    let newOrder = correctOrderingOfPorts oldOrder changedOrder
    let symbol' = {symbolToOrder with PortMaps = {symbolToOrder.PortMaps with Order = newOrder}}
    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    let newWires =
        //updateSymbolWires wModel symbolToOrder.Id
        {wModel with 
            Wires = wModel.Wires 
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }
    
    updateSymbolWires newWires symbolToOrder.Id