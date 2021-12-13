namespace AdventOfCode.Y2021

open Helpers

module Day13 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input13.txt")
    |> List.ofSeq
    |> List.filter (fun s -> s <> "")
    |> List.partition (fun s -> not (s.StartsWith "fold along"))
    |> fun (coords, folds) ->
          let coordsParsed =
            coords
            |> List.map (fun s ->
                s.Split ','
                |> List.ofArray
                |> List.map int
                |> function | [x; y] -> (x,y) | _ -> failwith "never"
                )
            |> List.toSet

          let foldsParsed =
            folds
            |> List.map (fun s ->
                s.Replace ("fold along ", "")
                |> fun s ->
                    if s.StartsWith "x=" then ('x', s.Replace("x=", "") |> int)
                    elif s.StartsWith "y=" then ('y', s.Replace("y=", "") |> int)
                    else failwith "never2"
                )
          (coordsParsed, foldsParsed)

  let mirror h foldH = if h < foldH then h else foldH - (h - foldH)

  let fold dots foldLine =
    dots
    |> Set.map (fun (x,y) ->
        if fst foldLine = 'x' then
            let foldLineX = snd foldLine
            let newX = mirror x foldLineX
            (newX, y)
        else
            let foldLineY = snd foldLine
            let newY = mirror y foldLineY
            (x, newY)
       )

  module Part1 =
    let go () =
      let dots, folds =  getInput ()

      let firstFold = List.head folds

      fold dots firstFold |> Set.count

  module Part2 =
    let go () =

      let dots, folds =  getInput ()

      let final = List.fold fold dots folds

      let printDots (dots : Set<int * int>) =
        let maxX = dots |> Set.map fst |> Set.maxElement
        let maxY = dots |> Set.map snd |> Set.maxElement
        for y in [0..maxY] do
          for x in [0..maxX] do
            if Set.contains (x,y) dots then printf "#" else printf "."
          printfn ""

      printDots final

  let run () =
    Part1.go () |> ps "Part 1: "

    printfn "\nPart2: "
    Part2.go () |> p
