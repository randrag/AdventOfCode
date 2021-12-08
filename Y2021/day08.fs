namespace AdventOfCode.Y2021

open Helpers

module Day08 =
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

    |> id
    |> pso "Parsed input: "

  module Part1 =
    let go () =
      getInput ()
      |> List.collect snd // get output values
      |> pso "Output values"
      |> List.map Set.count |> pso "Lengths: "
      |> List.filter (fun l -> (l = 2 || l = 3 || l = 4 || l = 7))
      |> List.length


  module Part2 =
    type Ident = | Unknown | Known of int | Possibly of List<int>

    let digits =
      [
        (0, "abcefg") // 6
        (1, "cf")  // 2
        (2, "acdeg") // 5
        (3, "acdfg")  // 5
        (4, "bcdf") // 4
        (5, "abdfg")  // 5
        (6, "abdefg")  // 6
        (7, "acf")  // 3
        (8, "abcdefg") // 8
        (9, "abcdfg")  // 6
      ]
      |> List.map (fun (n, l) -> (n, Seq.toSet l))

    let digits2 =
      [
          (1, "  c  f ")  // 2 - unique length
          (7, "a c  f ")  // 3 - unique length
          (4, " bcd f ")  // 4 - unique length
          (8, "abcdefg")  // 8 - unique length

          (3, "a cd fg")  // 5 // fully contains 1

          (5, "ab d fg")  // 5
          (2, "a cde g")  // 5

          (9, "abcd fg")  // 6
          (0, "abc efg")  // 6
          (6, "ab defg")  // 6
      ]

    let getLitSegmentsForIdent ident l =
      l |> List.filter (fun (set, identification) -> identification = ident) |> List.head |> fst


    let getLitSegmentsForNumber n (l : List<Set<char> * Ident>) =
      l |> List.filter (fun (set, identification) -> identification = Known n) |> List.head |> fst

    let go () =

      let input = getInput ()

      let firstId (litSegments : Set<char>) =
        let l = litSegments |> Set.count
        match l with
        | 2 -> litSegments, Known 1
        | 4 -> litSegments, Known 4
        | 3 -> litSegments, Known 7
        | 7 -> litSegments, Known 8
        | 5 -> litSegments, Possibly [2; 3; 5]
        | 6 -> litSegments, Possibly [0; 6; 9]
        | _ -> failwith "Bang2"

      // receives 10 signals' encodings
      let identify3 (l : List<Set<char> * Ident>) =
        let one = l |> getLitSegmentsForNumber 1

        let three =
          l
          |> List.filter (fun (s, ident) -> ident = Possibly [2; 3; 5])
          |> List.filter (fun (s, ident) -> Set.isSubset one s )
          |> pso "three: "
          |> List.head
          |> fst

        l
        |> List.map (fun (s, ident) ->
            if ident = Possibly [2; 3; 5] then
              if s = three then (s, Known 3) else (s, Possibly [2; 5])
            else (s, ident) )

      let identifySegmentE (l : List<Set<char> * Ident>) =
        let eight = l |> getLitSegmentsForNumber 8
        let three = l |> getLitSegmentsForNumber 3
        let four = l |> getLitSegmentsForNumber 4

        Set.difference eight three
        |> fun remaining -> Set.difference remaining four
        |> Set.minElement // there is only one
        |> pso "Segment E:"

      let identifyTwoAndFive l segmentE =
        l
        |> List.map (fun (s, ident) ->
            if ident = Possibly [2; 5] then if Set.contains segmentE s then (s, Known 2) else (s, Known 5)
            else (s, ident) )

      let identifyNine l segmentE =
        l
        |> List.map (fun (s, ident) ->
            if ident = Possibly [0; 6; 9] then if Set.contains segmentE s then (s, Possibly [0; 6]) else (s, Known 9)
            else (s, ident) )

      let identifyZeroAndSix l =
        let five = l |> getLitSegmentsForNumber 5
        l
        |> List.map (fun (s, ident) ->
            if ident = Possibly [0; 6] then
              let diff = Set.difference s five
              if Set.count diff = 2 then (s, Known 0)
              else (s, Known 6)
            else (s, ident)
           )

      let patterns =
        input
        |> List.map fst
        |> List.map (fun displays  -> displays |> List.map firstId)
        |> pso "Patterns: "

      let mappings =
        patterns
        |> List.map identify3
        |> pso "Three identified: "
        |> List.map (fun l ->
            let segmentE = identifySegmentE l
            identifyTwoAndFive l segmentE
            )
        |> List.map (fun l ->
            let segmentE = identifySegmentE l
            identifyNine l segmentE
            )
        |> pso "Two and three identified: \n"
        |> List.map identifyZeroAndSix
        |> List.map (List.sortBy snd)
        |> pso "Zero and six identified: \n"
        |> List.map (fun l -> l |> List.map (fun (s, ident) ->
            match ident with
            | Known n -> n, s
            | _ -> failwith "Bang3"
            ))
        |> List.map (List.sortBy fst)
        |> pso "Mapped: \n"
      ()

      let qq = input |> List.map snd // get output segments
      List.zip mappings qq
      |> List.map (fun (mapping, outputs) ->
          outputs
          |> List.map (fun litSegments ->
              let x =
                mapping
                |> List.filter (fun (n, set) -> set = litSegments)
                |> List.head
                |> fst
              x
        ))
      |> pso "numbers: "
      |> List.map(function | [a; b; c; d] -> a*1000 + b * 100 + c * 10 + d)
      |> List.sum


  let run () =
    Part1.go () |> ps "Part 1: "

    Part2.go () |> ps "Part 2: "

    // I think it would have been better to get all the permutations and to have tested them
    // 8! is only 40,000 or so.
