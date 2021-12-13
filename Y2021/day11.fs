namespace AdventOfCode.Y2021

open Helpers

module Day11 =
  let getInput () =
   System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input11_sample.txt")
    |> Seq.mapi (fun y s -> (y, s))
    |> Seq.collect (fun (y, s) ->
        s
        |> Seq.mapi (fun x c -> (x,y), c |> string |> int) )
    |> Map.ofSeq
    |> pso "Parsed input: "

  let getNeighbours (x, y) m =
      let nO  = ((x  , y-1) , Map.tryFind (x  , y-1) m)
      let nwO = ((x-1, y-1) , Map.tryFind (x-1, y-1) m)
      let wO  = ((x-1, y  ) , Map.tryFind (x-1, y  ) m)
      let swO = ((x-1, y+1) , Map.tryFind (x-1, y+1) m)
      let sO  = ((x  , y+1) , Map.tryFind (x  , y+1) m)
      let seO = ((x+1, y+1) , Map.tryFind (x+1, y+1) m)
      let eO  = ((x+1, y  ) , Map.tryFind (x+1, y  ) m)
      let neO = ((x+1, y-1) , Map.tryFind (x+1, y-1) m)
      [nO; nwO; wO; swO; sO; seO; eO; neO]
      |> List.map (fun (coord, vO) -> vO |> Option.map (fun v -> coord, v))
      |> List.choose id

  let printMapAndOutput m =
    for y in [0..9] do
      for x in [0..9] do
        printf "%i" ((Map.find (x,y) m) |> snd)
      printfn ""
    printfn ""
    m

  let printMapNeighboursAndOutput (neighbours, m) =
    printfn "Map: "
    printMapAndOutput m |> ignore
    ps "Neighbours: " neighbours
    (neighbours, m)

  let incrementCell (x,y) m =
    let cell = Map.find (x,y) m
    if cell + 1 = 10 then
      let neighbours = getNeighbours (x, y) m
      let newMap = Map.add (x,y) 0 m
      (neighbours, newMap)
    else
      let newMap = Map.add (x,y) (cell + 1) m
      ([], newMap)

  type Status = | NotFlashed | MustFlash | JustFlashed | Flashed

  let InitialAddStatus m = m |> Map.map' (fun v -> (NotFlashed, v))

  let step m =
    m
    |> Map.map' (fun (status, energy) ->
        match status, energy with
        | NotFlashed, 9 -> MustFlash, 0
        | JustFlashed, 0 -> Flashed, 0
        | NotFlashed, i -> NotFlashed, i + 1
        | _ -> failwith $"Unexpected status and energy {(status, energy)}"
        )

  let flash m =
    m
    |> Map.fold (fun l (x,y) (status, energy) ->          l ) []
    |> id



  module Part1 =
    let go () =
      getInput ()
      |> InitialAddStatus
      |> printMapAndOutput
      |> step
      |> printMapAndOutput


  module Part2 =
    let go () =
      ()

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
