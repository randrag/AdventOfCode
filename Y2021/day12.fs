namespace AdventOfCode.Y2021

open Helpers

module Day12 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input12.txt")
    |> id
    |> Seq.map (fun (s : string) -> s.Split '-' |> Array.toList |> function | [a;b] -> (a,b) | _ -> failwith "Never")
    |> Seq.collect (fun (from, to') -> if to' = "end" then [(from, to')] else [(from, to');(to', from)])
    |> Seq.toSet
    |> pso "Parsed input: "

  module Part1 =

    let isBigRoom (s : string) =
      let c = Seq.head s : char
      System.Char.IsUpper c

    let isSmallRoom (s : string) = not (isBigRoom s)

    let findPaths currentRoom visitedSmall connections =
      let availablePaths =
        connections
        |> Set.filter (fun (from, to') -> from = currentRoom)
        |> Set.filter (fun (from, to') -> not (Set.contains to' visitedSmall))
        |> pso "Available paths: "

      availablePaths

    type State = {
      CurrentRoom : string
      VisitedSmallRooms : Set<string>
      PathTakenRev : List<string>
      }


    let addPaths connections (state : State) : List<State> =
      if state.CurrentRoom <> "end" then
        let paths = findPaths state.CurrentRoom state.VisitedSmallRooms connections
        paths
        |> Set.toList
        |> List.map (fun (from, to') ->
            {
              CurrentRoom = to'
              VisitedSmallRooms = if isSmallRoom to' then Set.add to' state.VisitedSmallRooms else state.VisitedSmallRooms
              PathTakenRev = to'::state.PathTakenRev
              }
            )
      else [state]

    let rec findAllPaths states connections =
      let newStates =
        states
        |> List.collect (addPaths connections)
        |> List.distinct
      if List.toSet newStates = List.toSet states then newStates else findAllPaths newStates connections

    let go () =
      let connections = getInput ()
      //findPaths "start" connections Set.empty
      let states = [{CurrentRoom = "start"; VisitedSmallRooms = Set.singleton "start"; PathTakenRev = ["start"]}]
      findAllPaths states connections
      |> List.sortBy (fun state -> state.PathTakenRev)
      |> pso "List: "
      |> List.length


  module Part2 =
    let isBigRoom (s : string) =
      let c = Seq.head s : char
      System.Char.IsUpper c

    let isSmallRoom (s : string) = not (isBigRoom s)

    // receives a set of available connections
    // and a map of the number of times that a room has been visited
    // Must check which connections would be allowed and return them
    // if any small room has been visited twice, then a small room is only allowed if
    let findPaths currentRoom visitedSmallCounts connections =

      let prevMax = visitedSmallCounts |> Map.filter (fun room count -> room <> "start") |>  Map.fold' max 0
      //ps "visited: " (prevMax, visitedSmallCounts)
      let availableConnections =
        connections
        |> Set.filter (fun (from, to') -> from = currentRoom)
        |> Set.filter (fun (from, to') ->
            let visitedCount = Map.tryFind to' visitedSmallCounts |> Option.defaultValue 0
            if prevMax = 2 then visitedCount = 0 else visitedCount < 2 )

      availableConnections

    type State = {
      CurrentRoom : string
      VisitedSmallRooms : Map<string, int>
      PathTakenRev : List<string>
      }


    let addPaths connections (state : State) : List<State> =
      if state.CurrentRoom <> "end" then
        let paths = findPaths state.CurrentRoom state.VisitedSmallRooms connections
        paths
        |> Set.toList
        |> List.map (fun (from, to') ->
            {
              CurrentRoom = to'
              VisitedSmallRooms =
                if isSmallRoom to' then
                  let currentCount = Map.tryFind to' state.VisitedSmallRooms |> Option.defaultValue 0
                  Map.add to' (currentCount + 1) state.VisitedSmallRooms
                else state.VisitedSmallRooms
              PathTakenRev = to'::state.PathTakenRev
              }
            )
      else [state]

    let rec findAllPaths states connections =
      printf "."
      let newStates =
        states
        |> List.collect (addPaths connections)
        |> List.distinct
      printfn $"{List.length states} became {List.length newStates}"
      if List.length newStates = List.length states then newStates
      else findAllPaths newStates connections

    let go () =
      let connections = getInput ()
      //findPaths "start" connections Set.empty
      let states = [
        {
        CurrentRoom = "start"
        VisitedSmallRooms = ["start", 2] |> List.toMap
        PathTakenRev = ["start"]
      }]
      findAllPaths states connections
      |> List.map (fun state -> state.PathTakenRev |> List.rev)
      |> List.sort
      |> List.distinct

      |> pso "Paths taken: "
      |> List.length


  let run () =
    //Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
