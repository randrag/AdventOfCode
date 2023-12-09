namespace AdventOfCode.Y2023

open Helpers

module Day09 =

   // Part 1
   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.map (fun s -> s.Split(' ') |> Array.toList)
         |> List.map (List.map String.parseToIntX)

      let findDifferences (intL : List<int>) =
         intL
         |> List.pairwise
         |> List.map (fun (a, b) -> b - a)

      let listContainsAllZeroes (intL : List<int>) =
         intL
         |> List.forall (fun i -> i = 0)

      let  buildPyramid (intL : List<int>) =
         let rec inner (acc : List<List<int>>) (next : List<int>)  =
            let next' = findDifferences next
            if listContainsAllZeroes next' then
               next' :: next :: acc |> List.rev
            else
               inner (next :: acc) next'

         inner [] intL

      let printPyramidL p =
         p
         |> List.iter (fun pyramid ->
               pyramid
               |> List.iter (fun l ->
                     l |> List.iter (printf "%d ")
                     printfn ""
               )
               printfn ""
         )

      let go (year, day) runMode =
         let pyramids =
            getInput (year, day) runMode
            |> parse
            |> pso "Int lists: "
            |> List.map buildPyramid
            |> pso "Did scan: "
            |> fun p -> printPyramidL p; p

         pyramids
         |> List.map (fun pyramid ->
            pyramid
            |> List.map (fun line ->
               line |> List.last ) )
         |> List.map List.rev
         |> pso "Last numbers: "
         |> List.map (fun l ->
               l
               |> List.scan (fun acc x -> acc + x) 0
               |> List.tail
            )
         |> pso "Scanned: "
         |> List.map List.last
         |> List.sum
         |> pso "Summed: "




   // Part 2
   module Part2 =

      let printPyramid p =
         p
         |> List.iter (fun pyramid ->
            pyramid
            |> List.iter (fun l ->
               l
               |> List.iter (printf "%d ")
               printfn ""
            )
            printfn ""
         )

      let findFirst (inputL : List<int>) =
         let rec inner acc remaining =
            match remaining with
            | [] -> acc |> List.rev
            | x :: xs ->
               let prev = List.tryHead acc |> Option.defaultValue 0
               let next = x - prev
               inner (next :: acc) xs

         inner [] inputL

      let go (year, day) runMode =
         let pyramids =
            getInput (year, day) runMode
            |> Part1.parse
            |> pso "Int lists: "
            |> List.map Part1.buildPyramid
            |> fun p ->
                  printn "Pyramid: "
                  printPyramid p
                  p

         pyramids
         |> List.map (List.map List.head) // find first number of each line of each pyramid
         |> List.map List.rev
         |> pso "First numbers reversed: "
         |> List.map findFirst
         |> pso "Scanned: "
         |> List.map List.last
         |> List.sum
         |> pso "Summed: "

   // Run
   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

      let processL (input : List<int>) =
         let rec inner acc remaining =
            match remaining with
            | [] -> acc |> List.rev
            | x :: xs ->
               let prev = List.tryHead acc |> Option.defaultValue 0
               let next = x - prev
               inner (next :: acc) xs

         inner [] input

      [<Fact>]
      let t1 () =
         let input    = [0; 2; 0; 3; 10]
         let expected = [0; 2; -2; 5; 5]
         let actual = processL input
         test <@ actual = expected @>
