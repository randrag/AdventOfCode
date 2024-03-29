namespace AdventOfCode

open Helpers

module Day5 =
  let input = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day5Input.txt") |> List.ofSeq

  let toNumber l =
    let rec inner multiplier remaining =
      match remaining with
      | [] -> 0
      | x::xs -> x * multiplier  +  inner (multiplier * 2) xs

    inner 1 <| List.rev l

  let getId = Seq.map (fun c -> if (c = 'B') || (c = 'R') then 1 else 0) >> List.ofSeq >> toNumber

  let run() =
    input
    |> List.map getId
    |> fun l -> l |> List.max |> ps "The maximum seat is: "; l
    |> List.sort
    |> List.pairwise
    |> List.find (fun (a,b) -> b - a > 1)
    |> fun (a, _) -> a + 1
    |> ps "My seat id is: "
