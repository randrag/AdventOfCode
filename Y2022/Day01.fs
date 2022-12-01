namespace AdventOfCode.Y2022

open AdventOfCode
open Helpers

module Day01 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/input01.txt")
    |> Seq.map parseInt64O
    |> Seq.fold
         (fun state elem ->
            match elem with
            | None -> []::state
            | Some i64 ->
               let state' =
                 match state with
                 | [singleton] -> [i64::singleton]
                 | head::tail -> (i64::head)::tail
                 | [] -> state
               state'
       )
         [[]]

  module Part1 =
    let go () =
      getInput ()
      |> Seq.map Seq.sum
      |> Seq.max
      |> p

  module Part2 =
    let go () =
      getInput ()
      |> Seq.map Seq.sum
      |> Seq.sortDescending
      |> Seq.take 3
      |> Seq.sum
      |> p


  let run () =
    Part2.go ()
