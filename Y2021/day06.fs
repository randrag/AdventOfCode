namespace AdventOfCode.Y2021

open Helpers

module Day06 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input06.txt")
    |> Seq.head |> fun s -> s.Split(',') |> Array.toList
    |> List.map (fun ageString -> (int ageString, 1L)) // convert to int and add count of 1

  let ageAndSpawn (age, count) =
    if age > 0 then [(age-1, count)]
    else [(6, count); (8, count)]

  // tuple is (age, count)
  let rec ageNDays n ( l : List<int * int64> ) =
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
