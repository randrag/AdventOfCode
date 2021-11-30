namespace Y2015

module Day01 =
  let loadLines = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2015/input01.txt") |> Seq.concat
  let getMovement c = if c = '(' then 1  elif c = ')' then  -1  else failwith "Unexpected input"
  let applyMovement acc c = acc + getMovement c

  module Part1 =
    let floor () =  loadLines |> Seq.fold applyMovement 0

  module Part2 =
    let firstFloorIndex () = loadLines |> Seq.scan applyMovement 0 |> Seq.findIndex (fun i -> i < 0)

  module Run =
    let runner () =
      printfn $"Floor: {Part1.floor ()}"
      printfn $"Counter: {Part2.firstFloorIndex () }"
