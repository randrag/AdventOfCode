namespace AdventOfCode

module Day13 =
  let  input_a ()  = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input13a.txt")
  let  input_b () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input13b.txt")

  module Part1 =
    let run () =
      let input = input_b

      let startingTime =
        input ()
        |> Seq.head
        |> decimal

      let busList =
        input ()
        |> Seq.skip 1
        |> Seq.head
        |> fun s -> s.Split ','
        |> List.ofArray
        |> List.filter ((<>) "x")
        |> List.map decimal

      do
        busList
        |> List.map (fun busNumber -> startingTime / busNumber)
        |> List.map ceil
        |> List.zip busList
        |> List.map (fun (busNumber, multiple) -> busNumber, multiple, busNumber * multiple, (busNumber * multiple - startingTime) * busNumber)
        |> List.sortBy (fun (_, _, leavingTime, _) -> leavingTime)
        |> List.head
        |> fun (_, _, _, puzzleAnswer) -> ps "Answer: " puzzleAnswer

  module Part2 =
    let run () =

      let numList =
        input_b () |> Seq.skip 1 |> Seq.head
        |> fun s -> s.Split ','
        |> List.ofArray
        |> List.mapi (fun i s -> i,s)
        |> List.filter (fun (_,s) -> s <> "x")
        |> List.map (fun (i,s) ->  uint64 s, uint64 i) // now we have tuple of prime and offset

      let find (n1 : uint64, offset1: uint64) (n2, offset2) =
        let rec inner position =
          let x = position + offset2
          if ( x / n2) * n2 = x
          then position
          else inner (position + n1)

        inner offset1

      let rec loop list =
        match list with
        | [(n1, offset1); (n2, offset2)] ->
             find  (n1,  offset1) (n2, offset2)
        | (n1, offset1)::(n2, offset2)::xs ->
             let newStartAt = find   (n1, offset1)  (n2, offset2)
             loop ((n1 * n2, newStartAt)::xs)
        | l1 -> failwithf "Could not match %A" l1

      let ol = List.sortBy (fun (num, _) -> 0UL + num ) numList

      loop ol |>  fun answer -> ps "Answer 2: " answer

      ()

    ()
