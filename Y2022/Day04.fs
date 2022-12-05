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

      let fullyContains ((b1,e1), (b2,e2)) =
         (b1 <= b2 && e1 >= e2) || (b2 <= b1 && e2 >= e1)

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse
         |> List.map fullyContains
         |> List.sumBy (fun b -> if b then 1 else 0)

   module Part2 =

      let overlaps ((b1,e1),(b2,e2)) =
         not ( b2 > e1 || e2 < b1 )

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse
         |> List.map overlaps
         |> List.sumBy (fun b -> if b then 1 else 0)

   let run (year, day) =
      Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
