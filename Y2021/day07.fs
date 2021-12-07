namespace AdventOfCode.Y2021

open Helpers

module Day07 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input07.txt")
    |> Seq.head |> fun s -> s.Split(',') |> Array.toList |> List.map int

  module Part1 =
    let go () =
      let crabPositions = getInput ()
      let range = [(List.min crabPositions) .. (List.max crabPositions)]

      let calculateRequiredFuel targetPosition crabPosition = (targetPosition - crabPosition) |> abs
      let requiredFuelByPosition =
        range
        |> List.map (fun targetPosition ->
            let requiredFuels = crabPositions |> List.map ( calculateRequiredFuel targetPosition)
            (targetPosition, List.sum requiredFuels ))

      requiredFuelByPosition |> List.sortBy snd |> List.head

  module Part2 =
    let go () =
      let crabPositions = getInput ()
      let range = [(List.min crabPositions) .. (List.max crabPositions)]

      let calculateRequiredFuel targetPosition crabPosition =
        let dist = abs (targetPosition - crabPosition)
        (dist * (dist + 1)) / 2

      let requiredFuelByPosition =
        range
        |> List.map (fun targetPosition ->
            let requiredFuels = crabPositions |> List.map ( calculateRequiredFuel targetPosition)
            (targetPosition, List.sum requiredFuels ))

      requiredFuelByPosition |> List.sortBy snd |> List.head

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
