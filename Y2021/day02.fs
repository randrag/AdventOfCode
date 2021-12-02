namespace AdventOfCode.Y2021

open Helpers

module Day02 =
  let getInput () =
     System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input02.txt")
    |> Seq.map (fun s -> s.Split(' '))
    |> Seq.map (Seq.toList)
    |> Seq.map (fun l ->
      match l with
      | [dir; dis] -> dir, dis |> int64
      | _ -> failwith "bang"
      )

  module Part1 =
    let move (pos, depth) (direction, distance) =
      match direction with
      | "forward" -> (pos + distance, depth)
      | "down" -> (pos, depth + distance)
      | "up" -> (pos, depth - distance)
      | _ -> failwith "bang2"

    let go () =
      getInput ()
      |> Seq.fold move (0L, 0L)
      |> fun (a,b) -> a * b

  module Part2 =
    let move (pos, depth, aim) (direction, x) =
      match direction with
      | "down" -> (pos, depth, aim + x)
      | "up" -> (pos, depth, aim - x)
      | "forward" -> (pos + x, depth + aim * x, aim)
      | _ -> failwith "bang2"

    let go () =
      getInput ()
      |> Seq.fold move (0L, 0L, 0L)
      |> fun (a,b,_) -> a * b

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
