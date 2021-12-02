namespace AdventOfCode.Y2021

open Helpers

module Day01 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input01.txt")
    |> Seq.map int64

  module Part1 =
    let go () =
      getInput ()
      |> Seq.pairwise
      |> Seq.sumBy (fun (a,b) -> if a < b then 1 else 0)
      |> ps "Answer 1: "

  module Part2 =
    let go () =
      getInput ()
      |> Seq.windowed 3
      |> Seq.map Seq.sum
      |> Seq.pairwise
      |> Seq.sumBy (fun (a,b) -> if a < b then 1 else 0)
      |> ps "Answer 2: "

  let run () =
    Part1.go ()
    Part2.go ()
