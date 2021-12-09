namespace AdventOfCode.Y2021

open Helpers

module Day09 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input09.txt")
    |> Seq.mapi (fun y s -> (y, s))
    |> Seq.collect (fun (y, s) ->
        s
        |> Seq.mapi (fun x c -> (x,y), c |> string |> int) )
    |> Map.ofSeq
    |> pso "Parsed input: "

  let getNeighbours (x, y) m =
      let nO = Map.tryFind (x, y-1) m
      let wO = Map.tryFind (x-1, y) m
      let sO = Map.tryFind (x, y+1) m
      let eO = Map.tryFind (x+1, y) m
      [nO; wO; sO; eO] |> List.choose id

  module Part1 =
    let go () =
      let inputMap = getInput ()

      inputMap
      |> Map.map (fun (x, y) height ->
          let lowestSurrounding = getNeighbours (x, y) inputMap |> List.min
          if height < lowestSurrounding then (true, height) else (false, height)
        )
      |> Map.fold' (fun totalRisk (isLowPoint, height) ->
          if isLowPoint then totalRisk + 1 + height else totalRisk) 0
      |> pso "TotalRisk: "

  module Part2 =

    // number the low points
    // repeatedly fold through the map
    // if a point adjoins a low point and is not a 9, then it becomes part of the low region

    let go () =
      let inputMap = getInput ()

      let rec floodFill (m : Map<(int * int), (Option<int> * int)>) =
        let newMap =
          m
          |> Map.map
              (fun (x,y) (lpnO, height) ->
                 let neighbours = getNeighbours (x,y) m
                 let neighbouringBasins = neighbours |> List.map fst |> List.filter Option.isSome

                 if lpnO = None && height <> 9 && List.length neighbouringBasins > 0 then
                   let neighbouringLowPoint = List.head neighbouringBasins
                   (neighbouringLowPoint, height)
                 else (lpnO, height))

        if newMap = m then m
        else floodFill newMap

      inputMap
      |> Map.map (fun (x, y) height -> // find the low points
          let lowestSurrounding = getNeighbours (x, y) inputMap |> List.min
          if height < lowestSurrounding then (true, height) else (false, height))
      |> Map.fold // number the basins
           (fun (lowPointNumber, newMap) (x,y) (isLowPoint, height) ->
              if isLowPoint then
                let n = lowPointNumber + 1
                let m = newMap |> Map.add (x,y) (Some n, height)
                (n, m)
              else
                (lowPointNumber, newMap |> Map.add (x,y) (None, height)))
           (0, Map.empty)
      |> snd
      |> floodFill // expand the basins
      |> Map.toList
      |> List.groupBy (fun ((x,y),(basin,height) ) -> basin)
      |> List.choose (fun (basinO, l) ->
          match basinO with
          | Some n -> Some (List.length l)
          | None -> None)
      |> List.sortDescending
      |> List.take 3
      |> List.reduce (*)

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
