namespace AdventOfCode

module Day5 =
  let input = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day5Input.txt") |> List.ofSeq

  let toNumber l =
    let rec inner acc multiplier remaining =
      match remaining with
      | [] -> acc
      | x::xs -> inner (acc + x * multiplier) (multiplier * 2) xs

    inner 0 1 <| List.rev l

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
