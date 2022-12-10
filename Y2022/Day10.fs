namespace AdventOfCode.Y2022

open Helpers

module Day10 =
   type I = | NOP | ADDX of x : int64
   module Part1 =

      let parse (lines : seq<string>) =
         lines

         |> Seq.map (fun s-> s.Split " ")
         |> Seq.map Array.toList
         |> Seq.toList
         |> List.map (fun s ->
            match s with
            | ["noop"] -> NOP
            | ["addx"; si] -> ADDX (si |> String.parseToInt64X)
            )

      let rec runInstruction
         (
            acc : int64
            , acc2
            , remainingCyclesOnInstructionO  : Option<int>
            , cycleCount : int
         )
         (instruction : I)
         =
            //printfn $"\n cycle: {cycleCount}  x: {acc}  instruction: {instruction}  xxx: {remainingCyclesOnInstructionO}"
            let acc2' = Map.add cycleCount acc acc2
            match instruction with
            | NOP -> (acc, acc2', None, cycleCount + 1)
            | ADDX x ->
               match remainingCyclesOnInstructionO with
               | Some count ->
                  if count > 1 then
                     runInstruction (acc, acc2', Some (count - 1), cycleCount + 1) instruction
                  else (acc + x, acc2', None, cycleCount + 1)
               | None -> runInstruction (acc, acc2', Some 1, cycleCount + 1) instruction


      let go (year, day) runMode =

         let getMap () =
            let a,b,c,d =
               getInput (year, day) runMode
               |> parse
               |> List.fold
                     runInstruction
                     (1, Map.empty, None, 1)
            b

         [20; 60; 100; 140; 180; 220]
         |> List.map (fun n ->
            let x = Map.find n (getMap())
            x * (int64 n))
         |> pso "nnn: "
         |> List.sum

   module Part2 =

      let go (year, day) runMode =
         let getMap () =
            let a,b,c,d =
               getInput (year, day) runMode
               |> Part1.parse
               |> List.fold
                     Part1.runInstruction
                     (1, Map.empty, None, 1)
            b

         getMap ()
         |> Map.toList
         |> List.chunkBySize 40
         |> pso "Chunks\n"
         |> List.iter (fun line ->
               printfn ""
               line
               |> List.iter (fun (col, pos) ->
                     let col' = col % 40 - 1
                     if    col' - 1 <= (int pos)
                        && (int pos) <= col' + 1 then
                        printf "#"
                        else printf "."
                  ))


   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
