namespace AdventOfCode


module List2 =
    let innerJoin (l1 : List<'a>) (l2 : List<'b>) (compareFn : 'a -> 'b -> bool) =
      l1
      |> List.collect (fun e1 ->
           l2
           |> List.filter (compareFn e1)
           |> List.map (fun e2 -> e1, e2)
        )


module Day6 =
  let loadLines =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day6Input.txt")
    |> List.ofSeq
    |> List.map List.ofSeq


  let run () =
    loadLines
    |> List.splitMultipleOn ((=) [])
    |> List.map (List.fold (List.append) [] >> Seq.distinct >> Seq.length)
    |> List.sum
    |> ps "Part 1 answer: "

    let intersect l1 l2 =
      List2.innerJoin l1 l2 (=)
      |> List.map fst

    loadLines
    |> List.splitMultipleOn ((=) [])
    |> List.map (fun groupList -> List.fold intersect (List.head groupList) groupList) // don't like the List.head here
    |> List.map (List.length)
    |> List.sum
    |> ps "Part 2: "

