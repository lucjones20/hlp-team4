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
    let connectingOutputPorts = SmartHelpers.getSelectedSymbolWires wModel symbolToOrder otherSymbol
    let connectingInputPorts = SmartHelpers.getSelectedSymbolWires wModel otherSymbol symbolToOrder
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
            Wires = wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                                    // to make that happen the test function which calls this would need to provide an updateWire
                                    // function to this as a parameter (as was done in Tick3)
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }
    
    updateSymbolWires newWires symbolToOrder.Id