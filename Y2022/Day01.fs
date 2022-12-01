namespace AdventOfCode.Y2022

open Helpers

module Day01 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/input01.txt")
    |> Seq.fold
         (fun state elem ->
            match elem with
            | "" -> []::state
            | s ->
               let state' =
                 match state with
                 | [singleton] -> [s::singleton]
                 | head::tail -> (s::head)::tail
                 | [] -> state
               state'
       )
         [[]]

  module Part1 =
    let go () =
      getInput ()
      |> Seq.map (Seq.map  int64 >> Seq.sum)
      |> Seq.max
      |> p

  module Part2 =
    let go () =
      getInput ()
      |> Seq.map (Seq.map  int64 >> Seq.sum)
      |> Seq.sortDescending
      |> Seq.take 3
      |> Seq.sum
      |> p


  let run () =
    Part2.go ()
