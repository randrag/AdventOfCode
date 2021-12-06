namespace AdventOfCode.Y2021

open Helpers

module Day06 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input06.txt")
    |> Seq.head |> fun s -> s.Split(',') |> Array.toList |> List.map int64 |> List.map (fun age -> (age, 1L))

  let ageAndSpawn (age, count) = if age > 0L then [(age-1L, count)] else [(6L, count); (8L, count)]

  let rec ageNDays n ( l : List<int64 * int64> ) = // takes a list of (age, count)
    if n = 0 then l
    else
      l
      |> List.collect ageAndSpawn
      |> List.groupByAndMap fst snd
      |> List.map (fun (age, counts) -> age, (List.sum counts))
      |> ageNDays (n-1)

  let run () =
    getInput () |> ageNDays  80 |> List.sumBy snd |> ps "Part 1: "
    getInput () |> ageNDays 256 |> List.sumBy snd |> ps "Part 2: "
