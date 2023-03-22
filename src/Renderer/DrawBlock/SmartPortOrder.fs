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
    let connectingOutputPorts = getSelectedSymbolWires wModel symbolToOrder otherSymbol
    let connectingInputPorts = getSelectedSymbolWires wModel otherSymbol symbolToOrder
    let connectingOutputPortsIds = SmartHelpers.findPortIds connectingOutputPorts
    let connectingInputPortsIds = 
        SmartHelpers.findPortIds connectingInputPorts
        |> List.map (fun (x,y) -> y,x)
    let connectingPortsIds = connectingOutputPortsIds @ connectingInputPortsIds
    
    let orderEdge = 
        connectingPortsIds 
        |> List.map (fun (x,_) -> x) 
        |> List.map symbolToOrder.PortMaps.Orientation.TryFind
        |> List.choose id
    let staticEdge = 
        connectingPortsIds 
        |> List.map (fun (_,x) -> x) 
        |> List.map otherSymbol.PortMaps.Orientation.TryFind
        |> List.choose id
        
    let portsToGetOrderFrom = connectingPortsIds |> List.map (fun (_,x) -> x)
    let portsStaticByEdge = SmartHelpers.groupByEdge staticEdge portsToGetOrderFrom
    let optimalOrder = otherSymbol.PortMaps.Order
    let oldOrder = symbolToOrder.PortMaps.Order
    // Sorts non changing ports that are connected so the optimal order for the connected ports is found
    let sortedMapOfStaticPorts = SmartHelpers.sortPorts portsStaticByEdge optimalOrder 
    // list in order required for the static ports - makes it easier to sort the connectingPortsIds
    let sortedListOfStaticPorts = SmartHelpers.getListOfPortsFromMap sortedMapOfStaticPorts 
    
    let sortedConnectingPorts = SmartHelpers.sortTupleListByList connectingPortsIds sortedListOfStaticPorts
    let newOrderEdge = SmartHelpers.sortEdgeByList orderEdge sortedListOfStaticPorts connectingPortsIds
    
    let changedOrder = SmartHelpers.groupByEdge newOrderEdge sortedConnectingPorts

    let newOrder = SmartHelpers.correctOrderingOfPorts oldOrder changedOrder
    let symbol' = {symbolToOrder with PortMaps = {symbolToOrder.PortMaps with Order = newOrder}}
    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    let newWires =
        //updateSymbolWires wModel symbolToOrder.Id
        {wModel with 
            Wires = wModel.Wires 
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }
    
    updateSymbolWires newWires symbolToOrder.Id