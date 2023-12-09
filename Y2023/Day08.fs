namespace AdventOfCode.Y2023

open Helpers

module Day08 =

   // Part 1
   module Part1 =

      let parse (lines : seq<string>) =
         let lines =
            lines
            |> Seq.toList
            |> pso "Lines: "
         let lrlr =
            List.head lines
            |> String.toCharList

         let nodes =
            lines
            |> List.skip 2
            |> List.map (fun line ->
                  printfn "%s" line
                  let nodeName = line.Substring(0, 3)
                  let l,r =
                     line.Substring(7, 8)
                     |> String.splitOnceOnChar ','
                     |> fun (a,b) -> a.Trim(), b.Trim()

                  nodeName, (l, r)
               )
            |> List.toMap

         lrlr, nodes

      let run (lrlr : List<char>) nodes =

         let instructionCount = lrlr.Length

         let rec inner currentPos n (lrlr : Array<char>) =

            let nextDir = lrlr.[n % instructionCount]
            let nextPos =
               if nextDir = 'L' then nodes |> Map.find currentPos |> fst
               else nodes |> Map.find currentPos |> snd

            printfn "%A" nextPos
            if nextPos = "ZZZ" then n + 1 else inner nextPos (n+1) lrlr


         let lrlrArray = lrlr |> List.toArray
         inner "AAA" 0 lrlrArray





      let go (year, day) runMode =
         let lrlr, nodes =
            getInput (year, day) runMode
            |> parse

         run lrlr nodes


   // Part 2
   module Part2 =



      let run (lrlr : List<char>) nodes startingNode =

         let instructionCount = lrlr.Length

         let rec inner currentPos n (lrlr : Array<char>) =

            let nextDir = lrlr.[n % instructionCount]
            let nextPos =
               if nextDir = 'L' then nodes |> Map.find currentPos |> fst
               else nodes |> Map.find currentPos |> snd

            //printfn "%A" nextPos
            if (nextPos |> String.toCharList |> fun l -> l[2] = 'Z') then n + 1 else inner nextPos (n+1) lrlr


         let lrlrArray = lrlr |> List.toArray
         inner startingNode 0 lrlrArray


      let primeFactors (n: int64) =
         let rec findFactors n factor factors =
            if n <= 1L then factors
            elif n % factor = 0L then findFactors (n / factor) factor (factor :: factors)
            else findFactors n (factor + 1L) factors

         findFactors n 2L []
         |> List.rev

      let go (year, day) runMode =
         let lrlr, nodes =
            getInput (year, day) runMode
            |> Part1.parse

         let startingNodes =
            nodes
            |> Map.toListOfKeys
            |> List.filter (fun k -> k |> String.toCharList |> fun l -> l[2] = 'A')
            |> pso "Starting nodes: "

         startingNodes
         |> List.map (fun startingNode ->
               printfn "Starting node: %s" startingNode
               run lrlr nodes startingNode
            )
         |> pso "Results: "
         |> List.map int64
         |> List.collect primeFactors
         |> List.distinct
         |> List.reduce (*)
         |> pso "Answer: "
         |> ignore

         ()
         //run lrlr nodes

   // Run
   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

      //[<Fact>]
      //let t1 () =
      //   let input = [""]
      //   let expected = [""]
      //   let actual = parse input
      //   test <@ expected = actual @>
