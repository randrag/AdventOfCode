namespace AdventOfCode.Y2022

open Helpers

module Day07 =

   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse

   module Part2 =

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse

   let run (year, day) =
      Example |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
