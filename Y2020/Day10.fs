namespace AdventOfCode

module Day10 =

  let  input  = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input10.txt") |> List.ofSeq |> List.map int64

  let addWallAndDeviceJoltages l = List.concat [ [0L]; l; [List.max l + 3L] ]

  let run () =

    // Part 1
    input
    |> addWallAndDeviceJoltages
    |> List.sort
    |> List.pairwise
    |> List.map (fun (a,b) -> b - a) // get deltas
    |> List.countBy id
    |> List.filter (fun (a,_) -> a = 1L || a = 3L) // with provided data this step is not required
    |> List.map (fun (_, b) -> b)
    |> List.reduce ( * )
    |> ps "Part 1 answer: "

    // Part 2
    let canConnect j1 j2 = j2 > j1 && j2 - j1 <= 3L

    // Returns a list of each input value tupled with a list of the values you could jump to
    let addPossibleNextJoltages l = l |> List.map (fun j1 -> j1,  l |> List.filter (canConnect j1))

    let getPathCountsToDevice (joltages : List<int64>) =
      let rec inner  (downStreamPathCounts : Map<int64,int64>) (remainingList : List<int64 * List<int64>>) =
        match remainingList with
        | [] -> downStreamPathCounts |> Map.find 0L
        | (joltage, possibleNextDownstreamJoltages) :: xs ->
            let pathCountFromThisJoltage =
              possibleNextDownstreamJoltages
              // Look up downstream path count for each of the joltages we could use from our current joltage.
              // Default 1 for 'device' which is at very end of list.
              |> List.map (fun nextJoltage -> Map.tryFind nextJoltage downStreamPathCounts |> Option.defaultValue 1L )
              |> List.sum
              |> fun n -> if n = 0L then 1L else n // because 'device' has no downstream joltages
            inner (Map.add joltage pathCountFromThisJoltage downStreamPathCounts) xs

      joltages |> addPossibleNextJoltages |> List.sortBy (fun (j,_) -> -j) |> inner Map.empty

    input |> addWallAndDeviceJoltages |> getPathCountsToDevice |> ps "Part 2 answer: "
