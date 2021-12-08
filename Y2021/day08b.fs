namespace AdventOfCode.Y2021

open Helpers

module Day08B =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input08.txt")
    |> Seq.toList
    |> List.map (fun s ->
        s.Split('|') |> Array.toList
        |> List.map (fun s -> s.Split(' ') |> Array.toList)
        |> function
           | [uniqueSignalPatterns; outputValue] ->
               (uniqueSignalPatterns |> List.filter (fun s -> s <> "") |> List.map Set.ofSeq
                , outputValue |> List.filter (fun s -> s <> "") |> List.map Set.ofSeq)
           | _ -> failwith "Bang1"
        )
    |> pso "Parsed input: "

  module Part2 =
    let correctDigitsMap =
      [ "abcefg"; "cf"; "acdeg"; "acdfg"; "bcdf"; "abdfg"; "abdefg"; "acf" ; "abcdefg"; "abcdfg" ]
      |> List.mapi (fun i s -> (s |> Seq.toSet), i)
      |> List.toMap

    let correctDigitsSet = correctDigitsMap |> Map.toSetOfKeys

    let go () =
      let segmentLetters = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h']
      let permutations = List.allPermutations segmentLetters
      let fromToMaps =
        permutations
        |> List.map (fun permutation ->
            List.zip segmentLetters permutation
            |> Map.ofList)

      let input = getInput ()
      let digitsList =
        input
        |> List.map fst // only take the digits (the part before the |)
        |> List.map List.toSet // set of 10 digits each containing some segments

      // now transform the input in every permutation
      let maps =
        digitsList
        |> List.map (fun digits ->
          fromToMaps
          |> List.pick (fun fromToMap ->
              let mappedDigits =
                digits
                |> Set.map (fun litSegments ->
                    litSegments
                    |> Set.map (fun litSegment -> Map.find litSegment fromToMap))
              if mappedDigits = correctDigitsSet then Some fromToMap else None
            ))

      input
      |> List.map snd
      |> flip List.zip maps
      |> List.map (fun (litDigits, fromToMap) ->
          litDigits
          |> List.map (fun litSegments -> litSegments |> Set.map (fun segment -> Map.find segment fromToMap )))
      |> List.map (fun l -> l |> List.map (fun s -> Map.find s correctDigitsMap))
      |> List.sumBy (function | [a; b; c; d]  -> a * 1000 + b * 100 + c * 10 + d | _ -> failwith "bang4")

  let run () =
    Part2.go () |> ps "Part 2: "
