namespace AdventOfCode.Y2021

open Helpers

module DayXX =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/inputXX.txt")
    |> id
    |> pso "Parsed input: "

  module Part1 =
    let go () =
      getInput ()
      |> ignore

  module Part2 =
    let go () =
      ()

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
