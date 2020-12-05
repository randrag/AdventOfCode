namespace AdventOfCode
module Day5 =
  module Run =
    let input = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day5Input.txt") |> List.ofSeq
    let getRowInfo s = Seq.take 7 s |> Seq.map (fun c -> if c = 'B' then 1 else 0) |> List.ofSeq
    let getColumnInfo s = Seq.skip 7 s |> Seq.map (fun c -> if c = 'R' then 1 else 0) |> List.ofSeq

    let toNumber l =
      let rec inner acc multiplier remaining =
        match remaining with
        | [] -> acc
        | x::xs -> inner (acc + x * multiplier) (multiplier * 2) xs

      inner 0 1 <| List.rev l

    let getRow = getRowInfo >> toNumber
    let getColumn = getColumnInfo >> toNumber
    let getSeat s = getRow s, getColumn s
    let getSeatId (row, column) = row * 8 + column

    let run() =
      input |> List.map (getSeat >> getSeatId) |> List.max |> ps "The maximum seat is: "

      input
      |> List.map (getSeat >> getSeatId)
      |> List.sort
      |> List.pairwise
      |> List.filter (fun (a,b) -> b - a > 1)
      |> List.head
      |> fst
      |> (+) 1
      |> ps "My seat id is: "
