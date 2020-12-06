namespace AdventOfCode

module Day6 =

  let run () =

    let input = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day6Input.txt") |> List.ofSeq

    let stringToCharList = List.ofSeq

    input
    |> List.map stringToCharList
    |> List.splitMultipleOn ((=) [])
    |> List.map (List.reduce List.append >> List.distinct >> List.length)
    |> List.sum
    |> ps "Part 1 answer: "

    let intersect l1 l2 = l1 |> List.collect (fun e1 -> l2 |> List.filter ((=) e1))

    input
    |> List.map stringToCharList
    |> List.splitMultipleOn ((=) [])
    |> List.map (List.reduce intersect)
    |> List.map List.length
    |> List.sum
    |> ps "Part 2 answer: "
