namespace AdventOfCode.Y2021

open Helpers

// This was a rush job. Should be cleaned up somehow.

module Day03 =

  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input03.txt")
    |> Seq.map Seq.toList
    |> Seq.toList
    |> List.map (List.map (fun c -> if c = '1' then 1 else 0))

  let toDecimal bitsArray =

    let toDecimalFolder (total, multiplier) bitValue =
          (total + bitValue * multiplier, multiplier * 2)
    bitsArray |> Seq.rev |> Seq.fold toDecimalFolder (0,1) |> fst

  module Part1 =

    let countOnesZeroesInColumn bitsInColumn =
      bitsInColumn
      |> Seq.fold
          (fun (a, b) bit -> if bit = 1 then (a + 1, b) else (a, b + 1))
          (0,0)

    // Count the ones and zeroes in each column
    let getGammaEpsilon ssi =

      let counts =
        ssi
        |> List.transpose
        |> List.map countOnesZeroesInColumn

      let gamma =
        counts
        |> List.map (fun (a,b) -> if b > a then 1 else 0)
        |> toDecimal

      let epsilon =
        counts
        |> List.map (fun (a,b) -> if a > b then 1 else 0)
        |> toDecimal
        |> pso "epsilon: "

      (gamma, epsilon)


    let go () =
      let gamma, epsilon = getInput () |> getGammaEpsilon
      epsilon, gamma, epsilon * gamma

  module Part2 =
    let go () =

      let rec findRow bitPosition (remainingRows : seq<array<int>>) predF  =
        if remainingRows |> Seq.length = 1 then remainingRows |> Seq.head
        else
          let counts = // one entry per column
            remainingRows
            |> Seq.transpose
            |> Seq.map Part1.countOnesZeroesInColumn
            |> Seq.toArray

          let onesCount, zeroesCount = counts.[bitPosition]

          let nextRemainingRows =
            remainingRows
            |> Seq.toList
            |> List.filter (fun row ->
              let bit = row.[bitPosition]
              predF onesCount zeroesCount bit
              )

          findRow (bitPosition + 1) nextRemainingRows predF

      let input = getInput () |> Seq.map Seq.toArray
      let value1 =
        findRow
          0
          input
          (fun onesCount zeroesCount bit ->
            ((onesCount >= zeroesCount) && (bit = 1)) || (onesCount < zeroesCount) && (bit = 0))
        |> toDecimal

      let value2 =
        findRow
          0
          input
          (fun onesCount zeroesCount bit ->
            ((onesCount < zeroesCount) && (bit = 1)) || (onesCount >= zeroesCount) && (bit = 0))
        |> toDecimal

      value1 * value2

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
