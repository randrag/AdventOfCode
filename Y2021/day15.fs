namespace AdventOfCode.Y2021

open Helpers


type Position = int * int

type NodeStatus =
  | Visited of CostToReach : int
  | Considered of LowestFoundCostToReach : int
  | Unconsidered

type Node = {
  Position : Position
  CostToEnter : int
  Status : NodeStatus
  }


type State = {
  CurrentPosition : Position
  TargetPosition : Position
  Map : Map<Position, Node>
  }

module Day15 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input15_sample.txt")
    |> Seq.map Seq.toList|> Seq.toList
    |> List.mapi (fun y s -> s |> List.mapi (fun x c -> (x, y), c |> string |> int))
    |> List.concat
    |> List.toMap
    |> Map.map (fun position cost -> { Position = position; CostToEnter = cost; Status = Unconsidered })
    |> pso "Parsed input: "

  let findNeighbours (x,y) m =
    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
    |> List.map (flip Map.tryFind m)
    |> List.choose id

  let getVisitedCostToReach node =
    node.Status |> function | Visited costToReach -> costToReach | _ -> failwith "Node has not been visited"

  // adds current cost and entering cost to node being considered
  // returns none if node has previously been visited
  let considerNeighbour currentCost node =
    match node with
        | {CostToEnter = _; Status = Visited _} -> None
        | {CostToEnter = costToEnter; Status = Considered prevCost} as node ->
            let newLowestFoundCostToReach = min prevCost (currentCost + costToEnter)
            Some { node with Status = Considered newLowestFoundCostToReach }
        | {CostToEnter = costToEnter; Status = Unconsidered} as node ->
            Some { node with Status = Considered (currentCost + costToEnter) }

  // returns unvisited neighbours after considering them
  let considerNeighbours (x,y) m =
    let currentCost = Map.find (x,y) m |> getVisitedCostToReach
    findNeighbours (x,y) m
    |> List.map (considerNeighbour currentCost)
    |> List.choose id

  let getLowestCost node =
    () //node.Status |> function | Visited _ -> None

  module Part1 =
    let go () =
      let startingPos = (0,0)
      let m =
        getInput ()
        |> Map.change startingPos (function
            | Some node -> Some { node with Status = Visited 0 }
            | None -> failwith "Never"
            )

      let startingState = {
        CurrentPosition = (0,0)
        TargetPosition = (9,9)
        Map = m
        }

      let stepState state =
        let newNeighbours = considerNeighbours state.CurrentPosition state.Map
        let nextPos = newNeighbours |> List.minBy (fun node -> node.Status)
        ()

      considerNeighbours startingPos m
      |> pso "Neighbours after consideration: "
      |> List.fold (fun m node -> Map.add node.Position node m) m

  module Part2 =
    let go () =
      ()

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
