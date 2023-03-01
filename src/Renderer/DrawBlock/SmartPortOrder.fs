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

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)

// First need to check the output y of each of the ports leading into the symbolToOrder and then check the 
// corresponding outputs of the symbolToOrder - these should be stored in a list of tuples which should then be 
// ordered based on the outputs so the inputs are reordered into the correct ordering.

let findPortIds (wires: Map<ConnectionId,Wire>) : ((string * string) List) = 
    let matching { OutputPort = outPort; InputPort = inPort } = string outPort, string inPort
    wires
    |> Map.toSeq
    |> Seq.map (fun (_, x) -> x)
    |> Seq.toList
    |> List.map matching

let sorted (oldPorts: Map<Edge,string list>) (edge: Edge) (order: string list) (other: string list) =
    oldPorts
    |> Map.find edge
    |> List.sortBy (fun inPort -> 
            List.findIndex ((=) inPort) order 
            |> function 
               | -1 -> List.length other 
               | i -> i)


let sortPorts (oldPorts: Map<Edge,string list>) (optimalOrder: Map<Edge,string list>) : Map<Edge,string list> =
    optimalOrder 
    |> Map.fold (fun acc edge order -> 
            let other = 
                oldPorts
                |> Map.filter (fun e _ -> e <> edge)
                |> Map.toSeq
                |> Seq.map (fun (_, strlst) -> strlst)
                |> Seq.concat
                |> Seq.distinct
                |> List.ofSeq
            let sortedPorts = sorted oldPorts edge order other
            Map.add edge sortedPorts acc) 
        Map.empty

let groupByEdge (changingEdge: Edge list) (portsNeeded: string list) =
    let makeMapValue (inputList: (Edge*string)list) (edge:Edge) =
        let outputList = inputList |> List.map (fun (x,y) -> y)
        edge, (outputList |> List.rev)
    let splitList (originalList: (Edge*string)list) (edge:Edge)=
        originalList |> List.partition (fun (x,y) -> x = edge) |> (fun (x,y) -> makeMapValue x edge)
    let sortedList = (changingEdge,portsNeeded) ||> List.map2 (fun s1 s2 -> (s1,s2))
    Map.ofList [splitList sortedList Top ; splitList sortedList Bottom ; splitList sortedList Left ; splitList sortedList Right]

let extractListOfPorts (mapOfPorts:Map<Edge,string list>):string list =
    mapOfPorts
    |> Map.keys
    |> Seq.toList
    |> List.choose (fun edge ->
        match Map.tryFind edge mapOfPorts with
        | Some edges -> Some (edge, edges)
        | None -> None)
    |> List.sortBy (fun (edge, _) -> edge)
    |> List.collect snd

let correctOrderingOfPorts (originalList: Map<Edge, string list>) (correctOrderList: Map<Edge, string list>) : Map<Edge, string list> =
    originalList
    |> Map.fold (fun acc edge originalPorts -> 
        let correctPorts = Map.find edge correctOrderList
        let orderedPorts = SmartHelpers.correctOrderingOfList originalPorts correctPorts
        Map.add edge orderedPorts acc) Map.empty
    
let correctOrderingOfList (correctOrderList: string list) (originalList: string list): string list = 
    let wrongPorts = List.except correctOrderList originalList 
    let rec assembleList wrongPortList correctOrderList index acc = 
        match wrongPortList with
            |a::tail -> if originalList[index] = a then assembleList tail correctOrderList (index+1) (acc @ [a])
                        else acc @ correctOrderList @ wrongPortList
            | _ -> acc @ correctOrderList
    assembleList wrongPorts correctOrderList 0 []

let sortTupleListByNewList (tupleList: List<string*string>) (refList:string list) : string list =
    let refIndex = Map.ofList (List.mapi (fun i x -> (x, i)) refList)
    let sortByRefIndex ((x, y): string * string) =
        match refIndex.TryGetValue x with
        | true, index -> index, y
        | _ -> failwith "Element not found in reference list"
    tupleList |> List.sortBy sortByRefIndex |> List.map snd

let sortTupleListByList (tupleList: List<string*string>) (refList:string list) : string list =
    let swapList = tupleList |> List.map (fun (x,y) -> y,x)
    (sortTupleListByNewList swapList refList)

let sortEdgeByList (orderEdge: Edge list) (refList:string list) (tupleList: List<string*string>) : Edge list =
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
        : BusWireT.Model =
    let sModel = wModel.Symbol
    let connectingPorts = SmartHelpers.getSelectedSymbolWires wModel symbolToOrder otherSymbol 
    //let connectingPortsAway = SmartHelpers.getSelectedSymbolWires wModel otherSymbol symbolToOrder
    let connectingPortsIds = findPortIds connectingPorts
    //printfn $"connectingPortsIds: {connectingPortsIds}"
    //printfn $"Changing Ports: {symbolToOrder.PortMaps.Order}"
    //printfn $"Static Ports: {otherSymbol.PortMaps.Order}"
    
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
        
    let portsToOrder = connectingPortsIds |> List.map (fun (x,_) -> x)
    let portsToGetOrderFrom = connectingPortsIds |> List.map (fun (_,x) -> x)
    let portsStaticByEdge = groupByEdge staticEdge portsToGetOrderFrom
    let portsOrderingByEdge = groupByEdge orderEdge portsToOrder
    //printfn $"Grouped Static by edge: {portsStaticByEdge}"
    let optimalOrder = otherSymbol.PortMaps.Order
    let oldOrder = symbolToOrder.PortMaps.Order
    //printfn $"optimalOrder: {optimalOrder}"
    printfn $"connectingPortsIds: {connectingPortsIds}"
    let sortedMapOfStaticPorts = sortPorts portsStaticByEdge optimalOrder // Sorts non changing ports that are connected so the optimal order for the connected ports is found
    //now need to sort connecting ports so they are in the corresponding order
    //let sortedMapOfChangingPorts = sortPorts portsOrderingByEdge portsStaticByEdge
    //printfn $"sortedMapOfStaticPorts {sortedMapOfStaticPorts}"
    //printfn $"sortedMapOfChangingPorts {sortedMapOfChangingPorts}"
    let sortedListOfStaticPorts = extractListOfPorts sortedMapOfStaticPorts // list in order required for the static ports - makes it easier to sort the connectingPortsIds
    
    let sortedConnectingPorts = sortTupleListByList connectingPortsIds sortedListOfStaticPorts
    let newOrderEdge = sortEdgeByList orderEdge sortedListOfStaticPorts connectingPortsIds
    printfn $"sortedListOfStaticPorts {sortedListOfStaticPorts}"
    printfn $"sortedConnectingPorts {sortedConnectingPorts}"

    let changedOrder = groupByEdge newOrderEdge sortedConnectingPorts
    printfn $"changedOrder: {changedOrder}"
    printfn $"orderEdge: {orderEdge}"

    //let sortedPortsOld = (sortPorts connectingPortsIds optimalOrder)
    let newOrder = correctOrderingOfPorts oldOrder changedOrder // need to put sortedMapOfChangingPorts here instead
    printfn $"oldOrder {oldOrder}"
    printfn $"newOrder {newOrder}"
    let symbol' = {symbolToOrder with PortMaps = {symbolToOrder.PortMaps with Order = newOrder}}
    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    {wModel with 
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                                // to make that happen the test function which calls this would need to provide an updateWire
                                // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }