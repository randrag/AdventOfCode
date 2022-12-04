namespace AdventOfCode.Y2022

open Helpers
open Helpers.NoCaml

module Day04 =

   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.collect (fun line ->
               line.Split ','
               |> Array.toList
            )
         |> List.collect (fun halfLine ->
               halfLine.Split '-'
               |> Array.toList
               |> List.map String.parseToIntX
               )
         |> List.chunkBySize 4
         |> List.map (function
            | [a; b; c; d] -> (a,b),(c,d)
            | _ -> Unreachable ()
            )

      let fullyContains ((a,b), (c,d)) =
         (a <= c && b >= d)
         || (c <= a && d >= b)

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse
         |> List.map fullyContains
         |> List.sumBy (fun b -> if b then 1 else 0)

   module Part2 =

      let overlaps ((a,b),(c,d)) =
         not ( c > b || d < a)

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse
         |> List.map overlaps
         |> List.sumBy (fun b -> if b then 1 else 0)

   let run (year, day) =
      Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
