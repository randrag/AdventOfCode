namespace AdventOfCode.Y2021

open Helpers

module Day05 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input05.txt")
    |> Seq.toList
    |> List.map (fun (s : string) ->
        s.Split("->")
        |> Array.toList
        |> List.map (fun (s : string) ->
            s.Split ","
            |> Array.toList
            |> List.map int32
            |> function | [x;y] -> x, y  | _ -> failwith "bang1" )
        |> function | [startPoint;endPoint] -> startPoint, endPoint | _ -> failwith "bang2" )

  let countCoveredPoint pointMap (x,y) =
    let currentCount = pointMap |> Map.tryFind (x,y) |> Option.defaultValue 0
    pointMap |> Map.add (x,y) (currentCount + 1)

  module Part1 =
    let findPointsCoveredByLine ((x1, y1), (x2, y2)) =
      if x1 = x2 then
        let ys = if y1 <= y2 then [y1 .. y2] else [y2 .. y1]
        ys |> List.map (fun y -> (x1, y))
      elif y1 = y2 then
        let xs = if x1 <= x2 then [x1 .. x2] else [x2 .. x1]
        xs |> List.map (fun x -> (x, y1))
      else failwith "bang3"

    let go () =
      getInput ()
      |> List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
      |> List.collect findPointsCoveredByLine
      |> List.fold countCoveredPoint Map.empty
      |> Map.fold' (fun total count -> if count >= 2 then total + 1 else total) 0

  module Part2 =
    let findPointsCoveredByLine ((x1, y1), (x2, y2)) =
      if x1 = x2 then
        let ys = if y1 <= y2 then [y1 .. y2] else [y2 .. y1]
        ys |> List.map (fun y -> (x1, y))
      elif y1 = y2 then
        let xs = if x1 <= x2 then [x1 .. x2] else [x2 .. x1]
        xs |> List.map (fun x -> (x, y1))
      else
        let xs = if x1 <= x2 then [x1 .. x2] else [x1 .. -1 .. x2]
        let ys = if y1 <= y2 then [y1 .. y2] else [y1 .. -1 .. y2]
        List.zip xs ys

    let go () =
      getInput ()
      |> List.collect findPointsCoveredByLine
      |> List.fold countCoveredPoint Map.empty
      |> Map.fold' (fun total count -> if count >= 2 then total + 1 else total) 0

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
