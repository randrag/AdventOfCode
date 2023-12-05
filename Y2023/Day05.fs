namespace AdventOfCode.Y2023

open Helpers

module Day05 =

   // Part 1
   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse


   // Part 2
   module Part2 =

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse

   // Run
   let run (year, day) =
      Example |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

      [<Fact>]
      let t1 () =
         let input = 0
         let expected = 0
         let actual = 0
         test <@ expected = actual @>
