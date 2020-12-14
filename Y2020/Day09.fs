namespace AdventOfCode

module Day9 =
  let ro x = System.Console.ReadKey () |> ignore; x

  let  input  = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day9Input.txt")
                  |> List.ofSeq
                  |> List.map int64

  let getAllowed l1 =
    List.allPairs l1 l1
    |> List.filter (fun (a,b) -> a <> b)
    |> List.map (fun (a,b) -> a + b)

  let run () =
    let part1Answer =
      input
      |> List.windowed 26
      |> List.map (fun listToCheck ->
          match List.rev listToCheck with
          | newValue::previousValues ->
              let allowedNewValues = getAllowed previousValues
              let isValid = List.contains newValue allowedNewValues
              (isValid, newValue)
          | _ -> failwith "What happened?" )
      |> List.filter (fun (isValid, _) -> not isValid)
      |> List.head
      |> fun (_, b) -> b
      |> pso "Part 1 answer: "

    // Use sequences for lazyness and speed
    let windowLengths = [1.. List.length input - 1] |> Seq.ofList
    let input = Seq.ofList input
    windowLengths
    |> Seq.map (fun windowSize -> Seq.windowed windowSize input)
    |> Seq.map (fun ll ->
        ll
        |> Seq.map (fun candidateWindowL ->
            let smallest = Seq.min candidateWindowL
            let largest = Seq.max candidateWindowL
            (smallest+ largest, Seq.sum candidateWindowL)  )
        |> Seq.filter (fun (_,c) -> c =  part1Answer)
        )
    |> Seq.filter (Seq.isEmpty >> not)
    |> Seq.head
    |> ps "Answer: "

