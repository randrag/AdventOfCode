module Day3

open Microsoft.FSharp.Collections
open FsToolkit.ErrorHandling

module Common =

  let loadLines () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day3Input.txt")

  let parseLine (line : string) =
    line
    |> Seq.map (fun c -> c = '#')
    |> List.ofSeq

  let treemap =
    loadLines ()
    |> Seq.map parseLine
    |> List.ofSeq

module Part1 =
  let isTreeAtX (line : List<bool>) x =
    line.[x % (List.length line)]

  let isTreeAtXY treeMap (x, y) = option {
    let! row = treeMap |> List.tryItem y
    return isTreeAtX row x
    }

  let scanMap (dx, dy) treeMap : int =

    let rec inner acc (x, y) =
      match isTreeAtXY treeMap (x, y) with
      | Some true -> inner (acc + 1) (x + dx, y + dy)
      | Some false -> inner acc (x + dx, y + dy)
      | None -> acc

    inner 0 (dx, dy)

module Part2 =
  let slopes = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]


module Run =

    let run () =
      let treemap = Common.treemap

      let stopWatch = System.Diagnostics.Stopwatch.StartNew()

      let count = treemap |> Part1.scanMap (3, 1)

      printfn "In part 1 you will hit %i trees." count

      printfn "That took %f ms" stopWatch.Elapsed.TotalMilliseconds
      stopWatch.Restart()

      let product =
        Part2.slopes
        |> List.map (fun slope -> treemap |> Part1.scanMap slope |> fun i -> bigint i)
        |> List.fold (*) (bigint 1)

      printfn "In part 2 the product of trees hit on slopes is %A" product

      printfn "That took %f ms" stopWatch.Elapsed.TotalMilliseconds
      stopWatch.Stop()
