namespace AdventOfCode.Y2022

open Helpers

module Day06 =

   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.collect Seq.toList
         |> Seq.toList

      let calculate len ll =
         ll
         |> List.windowed len
         |> List.mapi (fun i l -> i+len, l, l |> List.toSet)
         |> List.filter (fun (_i, _l, set) ->  Set.count set = len)
         |> List.head

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse
         |> calculate 4

   module Part2 =

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse
         |> Part1.calculate 14

   let run (year, day) =
      Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
