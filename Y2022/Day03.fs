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
      |> Seq.toList

   module Part1 =
      let splitList (l : List<_>) =
        let length = List.length l
        let h1 = l |> List.take (length/2)
        let h2 = l |> List.skip (length/2)
        h1, h2

      let go () =
         getInput ()
         |> List.map (fun string ->
               string
               |> Seq.toList
               |> splitList
               |> fun (a, b) -> (
                     Set.intersect (a |> List.toSet) (b |> List.toSet)))
         |> List.collect (Set.map score >> Set.toList)
         |> List.sum

   module Part2 =

      let rec divideList (l1 : List<_>) (acc : List<List<_>>) =
         match l1 with
         | [] -> acc |> List.rev |> List.tail  // first element is an empty list
         | nonEmptyList ->
            divideList
               (nonEmptyList |> List.skip 3)
               ((nonEmptyList |> List.take 3)::acc)

      let go () =
         getInput ()
         // divide into groups of three
         |> fun l -> divideList l [[]]
         |> List.map (fun l -> // list of three strings
                  l
                  |> List.map Seq.toSet
                  |> List.reduce Set.intersect
               )
         |> List.sumBy (fun s -> s |> Set.toList |> List.sumBy score)

   let run () =
      Part1.go () |> p
      Part2.go () |> p
