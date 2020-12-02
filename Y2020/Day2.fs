module Day2

open System.Text.RegularExpressions

module Part1 =

  let loadLines () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day2Input.txt")

  let parseInput lines =
    lines
    |> Seq.map (fun line ->
        let m = Regex.Match (line, "^([0-9]+)[-]([0-9]+).(.)[:].([a-z]+)")

        let minCount = (m.Groups.[1].Value) |> int
        let maxCount = (m.Groups.[2].Value) |> int
        let char = (m.Groups.[3].Value) |> char
        let password = (m.Groups.[4].Value)

        (minCount, maxCount, char, password) )

  let loadAndParseInput = loadLines >> parseInput

  let countValidLines predicate lines =
    lines
    |> Seq.filter predicate
    |> Seq.length


  let part1 =

    let isLineValid (minCount, maxCount, char, password : string) =
      let count = password |> Seq.filter ((=) char) |> Seq.length
      (count >= minCount) && (count <= maxCount)

    loadAndParseInput () |> countValidLines isLineValid

  let part2 =

    let isLineValid (pos1, pos2, char, password : string) =
      let charList = [password.[pos1-1]; password.[pos2-1]]
      charList |> List.filter ((=) char) |> List.length |> (=) 1

    loadAndParseInput () |> countValidLines isLineValid



