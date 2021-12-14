namespace AdventOfCode.Y2021

open System
open Helpers


module Day14 =
  let getInput () =
    let lines =
      System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input14.txt") |> List.ofSeq

    let template = lines |> List.head

    let rules =
      lines
      |> List.skip 2
      |> List.map (fun s ->
          s.Split " -> "
          |> Array.toList
          |> function
               | [a;b] ->
                   let letters = (Seq.head a, Seq.tail a |> Seq.head)
                   (letters, b |> Seq.head)
               | _ -> failwith "never")

    (template |> Seq.map id |> Seq.toList, rules)

  module Part1 =

    let insert pair (pairToMatch, letterToInsert) =
      if pair = pairToMatch then Some [fst pair; letterToInsert; snd pair] else None

    let findAndInsert  (rules : List<(char * char) * char>) (pair  : char * char) =
      rules
      |> List.choose (insert pair)
      |> List.tryHead
      |> Option.defaultValue [fst pair; snd pair] // no matching rule

    let rec reassemble (acc : List<char>) (l: List<List<char>>) =
      if acc = [] then
        let newAcc = List.head l
        reassemble newAcc (List.tail l)
      else
        match l with
        | [] -> failwith "never"
        | [charList] ->
            let listToAppend = charList |> List.skip 1
            List.append acc listToAppend
        | charList :: charLists ->
            let listToAppend = charList |> List.skip 1
            let newAcc = List.append acc listToAppend
            reassemble newAcc charLists

    let step polymer rules =
      polymer
      |> List.pairwise
      |> List.map (findAndInsert rules)
      |> reassemble []

    let rec stepN remainingCount polymer rules =
      remainingCount |> ps "count: "
      if remainingCount > 0 then stepN (remainingCount - 1) (step polymer rules) rules
      else polymer

    let go () =
      let start, rules =  getInput ()

      let finalPolymer =
        stepN 10 start rules

      finalPolymer
      |> List.groupBy id
      |> List.map (fun (c, l) -> (c, List.length l))
      |> List.sortBy snd

  module Part2 =

    let go () =
      let start, rules =  getInput ()

      let addPairToMap m (pair : char * char, countToAdd : int64) =
        let currentCount = Map.tryFind pair m |> Option.defaultValue 0L
        Map.add pair (currentCount + countToAdd) m

      let startingPairMap =
        start
        |> Seq.pairwise
        |> Seq.map (fun tuple -> tuple, 1L)
        |> Seq.fold addPairToMap Map.empty

      let getRuleResult rule =
        let c1, c2 = rule |> fst
        let c3 = rule |> snd
        [(c1, c3), 1L; (c3, c2), 1L; (c1, c2), -1L]

      let getAdjustmentsForRule m rule =
        let currentCount = Map.tryFind (fst rule) m |> Option.defaultValue 0L
        rule
        |> getRuleResult
        |> List.map (fun (a,n) -> a, n * currentCount)

      let getAdjustmentsForRules m rules =
        rules
        |> List.collect (getAdjustmentsForRule m)
        |> List.groupByAndMap fst snd
        |> List.map (fun (a,b) -> a, List.sum b)

      let step m rules =
        getAdjustmentsForRules m rules
        |> List.fold addPairToMap m

      let rec stepN remainingCount m rules =
        if remainingCount > 0 then stepN (remainingCount - 1) (step m rules) rules
        else m

      stepN 40 startingPairMap rules
      |> Map.toList
      |> List.collect (fun ((c1, c2), count) -> [(c1, count);(c2, count)])
      |> fun l -> // we are now double counting, except missing the start and end characters once
          let p1 = (start |> List.head, 1L)
          let p2 = (start |> List.last, 1L)
          List.concat2 [p1; p2] l
      |> List.groupByAndMap fst snd
      |> List.map (fun (a,b) -> a, (List.sum b) / 2L) // could drop the letter here, leave for debugging
      |> List.sortBy snd
      |> fun l ->
           let n1 = l |> List.head |> snd
           let n2 = l |> List.last |> snd
           n2 - n1

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
