namespace AdventOfCode.Y2022

open Helpers


module Day03 =

   type System.Char with
      member c.toInt = int c

   let score (c : char) =
      if 'a'.toInt <= c.toInt && c.toInt <= 'z'.toInt
      then c.toInt - 'a'.toInt + 1
      else c.toInt - 'A'.toInt + 27

   let getInput () =
      System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/input03.txt")

   module Part1 =

      let go () =
         getInput ()
         |> Seq.collect (fun string ->
               string
               |> Seq.splitInto 2
               |> Seq.map Seq.toSet
               |> Seq.reduce Set.intersect
               |> Set.toSeq
            )
         |> Seq.sumBy score

   module Part2 =

      let go () =
         getInput ()
         |> Seq.chunkBySize 3
         |> Seq.collect (fun l -> // list of three strings
               l
               |> Seq.map Seq.toSet
               |> Seq.reduce Set.intersect
               |> Set.toList
            )
         |> Seq.sumBy score

   let run () =
      Part1.go () |> p
      Part2.go () |> p
