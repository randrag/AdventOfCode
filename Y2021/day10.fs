namespace AdventOfCode.Y2021

open Helpers

module Day10 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input10.txt")
    |> Seq.map Seq.toList |> Seq.toList

  module Part1 =
    let go () =

      let rec isCorrupted stack remaining =
        match remaining, stack with
        | '['::rs, _ -> isCorrupted ('['::stack) rs
        | '<'::rs, _ -> isCorrupted ('<'::stack) rs
        | '('::rs, _ -> isCorrupted ('('::stack) rs
        | '{'::rs, _ -> isCorrupted ('{'::stack) rs
        | ']'::rs, '['::ss -> isCorrupted ss rs
        | ')'::rs, '('::ss -> isCorrupted ss rs
        | '}'::rs, '{'::ss -> isCorrupted ss rs
        | '>'::rs, '<'::ss -> isCorrupted ss rs
        | r::_, _ -> false, Some r
        | _ -> true, None

      let scores = [(')',3); (']',57); ('}',1197); ('>',25137)] |> Map.ofList

      getInput ()
      |> List.choose (isCorrupted [] >> snd)
      |> List.sumBy (fun c -> Map.find c scores)

  module Part2 =
    let go () =

      let matching = [('{', '}'); ('[', ']'); ('(', ')'); ('<', '>'); ] |> Map.ofList

      let rec findIncomplete stack remaining =
        match remaining, stack with
        | '['::rs, _ -> findIncomplete ('['::stack) rs
        | '<'::rs, _ -> findIncomplete ('<'::stack) rs
        | '('::rs, _ -> findIncomplete ('('::stack) rs
        | '{'::rs, _ -> findIncomplete ('{'::stack) rs
        | ']'::rs, '['::ss -> findIncomplete ss rs
        | ')'::rs, '('::ss -> findIncomplete ss rs
        | '}'::rs, '{'::ss -> findIncomplete ss rs
        | '>'::rs, '<'::ss -> findIncomplete ss rs
        | [], stack -> Some stack
        | _ -> None // Corrupted - return none

      let scores = [(')',1L); (']',2L); ('}',3L); ('>',4L)] |> Map.ofList

      let score acc c = acc * 5L + (Map.find c scores)

      let l1 =
        getInput ()
        |> List.choose (findIncomplete [])
        |> List.map (List.map (fun c -> Map.find c matching))
        |> List.map (List.fold score 0L)
        |> List.sort

      let length = List.length l1
      List.skip (length / 2) l1 |> List.head

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
