namespace AdventOfCode.Y2022

//open AdventOfCode
open Helpers
open NoCaml

module Day01 =

   let getInput () =
      System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/example01.txt")
      |> Seq.toList
      |> List.splitMultipleOnExcl (fun s -> s = "")
      |> List.map (List.map String.parseToInt64X)

   module Part1 =
      let go () =
         getInput ()
         |> Seq.map Seq.sum
         |> Seq.max

   module Part2 =
      let go () =
         getInput ()
         |> Seq.map Seq.sum
         |> Seq.sortDescending
         |> Seq.take 3
         |> Seq.sum

   let run () =
      Part1.go () |> printn
      Part2.go () |> printn
