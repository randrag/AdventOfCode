namespace AdventOfCode.Y2022

open Helpers.Helpers


module DayXX =

   let getInput () =
      System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/inputXX.txt")

   module Part1 =

      let go () =
         getInput ()

   module Part2 =

      let go () =
         getInput ()

   let run () =
      Part1.go () |> printn
      Part2.go () |> printn
