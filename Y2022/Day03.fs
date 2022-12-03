namespace AdventOfCode.Y2022

open Helpers.Helpers


module Day03 =

   let getInput () =
      System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/input03.txt")

   type System.Char with
      member c.toInt = int c

   let score (c : char) =
      if 'a'.toInt <= c.toInt && c.toInt <= 'z'.toInt
      then c.toInt - 'a'.toInt + 1
      else c.toInt - 'A'.toInt + 27

   let findCommonChars (s : seq<_>) =
      s
      |> Seq.map Helpers.Seq.toSet
      |> Seq.reduce Set.intersect
      |> Set.toSeq

   module Part1 =

      let go () =
         getInput ()
         |> Seq.collect (Seq.splitInto 2 >> findCommonChars)
         |> Seq.sumBy score

   module Part2 =

      let go () =
         getInput ()
         |> Seq.chunkBySize 3
         |> Seq.collect findCommonChars
         |> Seq.sumBy score

   let run () =
      Part1.go () |> printn
      Part2.go () |> printn
