namespace AdventOfCode.Y2021

open Helpers


type Position = int * int

type UnvisitedNode = {
  Position : Position
  CostToEnter : int
  LowestKnownCostToReach : int
  }

type VisitedNode = {
  Position : Position
  CostToEnter : int
  LowestCostToReach : int
}

type State = {
  CurrentPosition : Position
  TargetPosition : Position
  UnvisitedNodes : Map<Position, UnvisitedNode>
  VisitedNodes : Map<Position, VisitedNode>
  }

module Day15 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input15.txt")
    |> Seq.map Seq.toList|> Seq.toList
    |> List.mapi (fun y s -> s |> List.mapi (fun x c -> (x, y), c |> string |> int))
    |> List.concat
    |> List.toMap
    |> Map.map (fun position cost ->
        { Position = position; CostToEnter = cost; LowestKnownCostToReach = System.Int32.MaxValue })


  let findNeighbours (x,y) m =
    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
    |> List.map (flip Map.tryFind m)
    |> List.choose id

  /// adds current cost and entering cost to node being considered
  /// Updates the lowest known cost if the new cost is lower
  let considerUnvisitedNeighbour currentCost unvisitedNode =
    let newCost = min unvisitedNode.LowestKnownCostToReach (currentCost + unvisitedNode.CostToEnter)
    { unvisitedNode with LowestKnownCostToReach = newCost }

  /// Finds unvisited neighbours
  /// Updates them to take current node into account
  /// Returns them so caller can handle them
  let updateUnvisitedNeighbours (currentNode : VisitedNode) (allUnvisitedNodes : Map<int * int, UnvisitedNode>) =
    findNeighbours currentNode.Position allUnvisitedNodes
    |> List.map (considerUnvisitedNeighbour currentNode.LowestCostToReach)
    |> List.fold
        (fun m unvisitedNode -> Map.add unvisitedNode.Position unvisitedNode m)
        allUnvisitedNodes

  module UnvisitedNode =
    let toVisitedNode (unvisitedNode : UnvisitedNode) = {
          Position = unvisitedNode.Position
          CostToEnter = unvisitedNode.CostToEnter
          LowestCostToReach = unvisitedNode.LowestKnownCostToReach
      }

  /// fetches the unvisited node at the current position
  /// converts it to visited and updates the maps
  /// returns it and the maps
  let getCurrentNodeAndUpdateMaps (pos : Position) unvisitedNodes visitedNodes =
    let currentNode = Map.find pos unvisitedNodes
    let asVisited = UnvisitedNode.toVisitedNode currentNode
    (asVisited, Map.add pos asVisited visitedNodes, Map.remove pos unvisitedNodes)

  // expects rectangular grid
  let getMaxPos m =
    m
    |> Map.toSetOfKeys
    |> Set.fold (fun (xMax,yMax) (x,y) -> (max x xMax, max y yMax)) (0,0)

  let getSize m = m |> getMaxPos |> fun (x,y) -> x + 1, y + 1

  let printMaps (m1 : Map<Position, UnvisitedNode>) (m2 : Map<Position, VisitedNode>) =
    for y in [0..49] do
      for x in [0..49] do
        let v =
          Map.tryFind (x,y) m1
          |> Option.map (fun n -> n.CostToEnter)
          |> Option.map string
          |> Option.defaultValue
               (Map.tryFind (x,y) m2
                |> Option.map (fun n -> n.CostToEnter)
                |> Option.map string
                |> Option.defaultValue " ")
        printf "%s" v
      printfn ""

  let rec search state count =
    if count % 100 = 0 then
      ps "Entering with count, pos: " (count, state.CurrentPosition)
    //printfn ""
    //printMaps Map.empty state.VisitedNodes
    //System.Console.ReadKey() |> ignore
    // fetch current node, make it be visited, update maps
    let currentNode, visitedNodes, unvisitedNodes =
      getCurrentNodeAndUpdateMaps state.CurrentPosition state.UnvisitedNodes state.VisitedNodes

    if currentNode.Position = state.TargetPosition then currentNode
    else
      // update any unvisited neighbours
      let updatedUnvisitedNodes = updateUnvisitedNeighbours currentNode unvisitedNodes
      // find next visitable node if one exists
      let NextNodeO, finalCost =
        updatedUnvisitedNodes
        |> Map.fold'
             (fun (nodeO, lowest) node ->
                if node.LowestKnownCostToReach < lowest then Some node, node.LowestKnownCostToReach
                else (nodeO, lowest))
             (None, System.Int32.MaxValue)
      match NextNodeO with
      | Some nextNode ->
          search
            {
            CurrentPosition = nextNode.Position
            TargetPosition = state.TargetPosition
            UnvisitedNodes = updatedUnvisitedNodes
            VisitedNodes = visitedNodes
            }
            (count + 1)
      | None -> failwith "No path to target node"



  module Part1 =
    let go () =
      let startingPos = (0,0)
      let startingUnvisitedNodes =
        getInput ()
        |> Map.change startingPos (fun nO -> nO |> Option.map (fun n -> {n with LowestKnownCostToReach = 0} ))

      let targetPosition =
        startingUnvisitedNodes
        |> Map.toSetOfKeys
        |> Set.fold (fun (xMax,yMax) (x,y) -> (max x xMax, max y yMax)) (0,0)

      let startingState = {
        CurrentPosition = (0,0)
        TargetPosition = targetPosition
        UnvisitedNodes = startingUnvisitedNodes
        VisitedNodes = Map.empty
        }


      search startingState 0

  module Part2 =
    let go () =
      let startingPos = (0,0)
      let smallMap = getInput ()
      let xSize, ySize = getSize smallMap |> pso "Sizes: "
      let multipliers =
        [0 .. 4] |> List.collect (fun y -> [0..4] |> List.map (fun x -> x,y)) |> pso "multipliers: "

      let newMap =
        smallMap
        |> Map.toList
        |> List.collect (fun ((x,y), unvisitedNode ) ->
              multipliers
              |> List.map (fun (xm, ym) ->
                  let cost =
                    unvisitedNode.CostToEnter + xm + ym
                    |> fun cost -> if cost > 9 then cost - 9 else cost
                  let pos = (x + xSize * xm, y + ySize * ym)
                  pos, { unvisitedNode with Position = pos; CostToEnter = cost }
                  ))
        |> List.sortBy fst
        |> Map.ofList


      //printMaps newMap Map.empty

      let startingUnvisitedNodes =
        newMap
        |> Map.change startingPos (fun nO -> nO |> Option.map (fun n -> {n with LowestKnownCostToReach = 0} ))

      let targetPosition = startingUnvisitedNodes |> getMaxPos |> pso "targetPosition: "

      let startingState = {
        CurrentPosition = startingPos
        TargetPosition = targetPosition
        UnvisitedNodes = startingUnvisitedNodes
        VisitedNodes = Map.empty
        }

      search startingState 0


  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "

// 11637517422274862853338597396444961841755517295286
// 11637517422274862853338597396444961841755517295286


// 67554889357866599146897761125791887223681299833479
// 67554889357866599146897761125791887223681299833479
