namespace AdventOfCode.Y2021

open Helpers

module Day11 =
  let getInput () =
   System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input11.txt")
    |> Seq.mapi (fun y s -> (y, s))
    |> Seq.collect (fun (y, s) ->
        s
        |> Seq.mapi (fun x c -> (x,y), c |> string |> int) )
    |> Map.ofSeq

  type Status =
    | UnFlashed of FlashCount : int * Value : int
    | MustFlash of FlashCount : int
    | Flashed   of FlashCount : int

  let printMapAndOutput s m =
    printfn $"{s}: \n"
    for y in [0..9] do
      for x in [0..9] do
        let value = Map.find (x,y) m
        let c =
          match value with
          | UnFlashed (flashcount, value) -> value |> string |> Seq.head
          | Flashed _ -> 'F'
          | MustFlash _ -> 'M'
        printf $"{c}"
      printfn ""
    printfn ""
    m

  let getNeighbours (x, y) m =
      [ (x-1, y-1); (x, y-1); (x+1, y-1);  (x-1, y); (x+1, y); (x-1, y+1); (x, y+1); (x+1, y+1);  ]
      |> List.map (fun pos -> Map.tryFind pos m |> Option.map (fun v -> pos, v))
      |> List.choose id

  let addInitialStatus m = m |> Map.map' (fun value -> UnFlashed (0, value))

  let incrementCell value =
    match value with
    | UnFlashed (c, 9) -> MustFlash c
    | UnFlashed (c, n) -> UnFlashed (c, (n + 1))
    | MustFlash c -> MustFlash c
    | Flashed c -> Flashed c

  let flashCell pos m = // outputs a new map
    let cellValue = Map.find pos m
    match cellValue with
    | MustFlash flashCount ->
      let neighbours = getNeighbours pos m
      let incrementedNeighbours = neighbours |> List.map (fun (pos, v) -> (pos, incrementCell v))
      incrementedNeighbours
      |> List.fold (fun m (pos, status) -> Map.change pos (fun _ -> Some status) m) m
      |> Map.change pos (fun _ -> Some (Flashed (flashCount + 1)))

    | _ -> failwith "Cell should not be flashed"

  let rec flashMap m =
    m
    |> Map.tryPick (fun position value -> match value with | MustFlash _ -> Some position | _ -> None)
    |> function
        | Some positionToFlash ->
            m
            |> flashCell positionToFlash
            |> flashMap
        | None -> m

  // Reset map after everything that must flash has flashed
  let resetMap m =
    m
    |> Map.map' (fun cellValue ->
         match cellValue with
         | UnFlashed (_, n) -> cellValue
         | Flashed flashCount -> UnFlashed (flashCount, 0)
         | MustFlash flashCount -> failwith "bug"
         )

  let step m =
    m
    |> Map.map' incrementCell
    |> flashMap
    |> resetMap

  let rec stepN n m =
    if n = 0 then m else stepN (n - 1) (step m)

  let getTotalFlashes m =
    m
    |> Map.toList
    |> List.map snd
    |> List.sumBy (function
        | UnFlashed (flashCount, _) -> flashCount
        | _ -> failwith "bug" )


  module Part1 =
    let go () =
      getInput ()
      |> addInitialStatus
      |> printMapAndOutput "Initial"
      |> stepN 100
      |> printMapAndOutput "Stepped"
      |> getTotalFlashes

  module Part2 =
    let rec stepUntilAllFlash n m =
      ps "Entering stepUntilAllFlash with n = " n
      let size = Map.count m
      let flashCountBefore = getTotalFlashes m
      let m = step m
      let flashCountAfter = getTotalFlashes m
      if (flashCountAfter - flashCountBefore) = size then (n + 1) else stepUntilAllFlash (n + 1) m

    let go () =
      getInput ()
      |> addInitialStatus
      |> stepUntilAllFlash 0

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
