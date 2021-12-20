namespace AdventOfCode.Y2021

open Helpers

module Day18 =

  let charListToString (chars : List<char>) =
    System.String.Concat(Array.ofList(chars))

  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input18_sample.txt")
    |> Seq.map Seq.toList
    |> Seq.toList


  let isNumber c = List.contains c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

  let splitAtComma chars =
    let chars = chars |> List.rev |> List.tail |> List.rev |> List.tail // drop first and last elements (brackets)

    let rec findCommaPosition (position, nestingLevel) chars =
      match chars with
      | c::cs when isNumber c -> findCommaPosition (position + 1, nestingLevel) cs
      | '['::cs -> findCommaPosition (position + 1, nestingLevel + 1) cs
      | ']'::cs -> findCommaPosition (position + 1, nestingLevel - 1) cs
      | ','::cs -> if nestingLevel = 0 then position else findCommaPosition (position + 1, nestingLevel) cs
      | _ -> failwith "Never"

    let commaPosition = findCommaPosition (0,0) chars

    chars
    |> List.splitAt commaPosition
    |> fun (a,b) -> a, List.tail b // drop comma

  let printMyTupleListAndOutput tl =
    tl |> List.iter (fun (a, b) -> printf $"\nnext: (   {charListToString a}  and  {charListToString b}   )\n")
    tl

  type SfNumber =
    | P of SfNumber * SfNumber // P = pair
    | L of int32 // L = Literal


  let parse chars =

    let rec innerParse chars =
      let a, b = splitAtComma chars
      let a' = a |> function | [c] -> L (c |> string |> int32) | l -> P (innerParse l)
      let b' = b |> function | [c] -> L (c |> string |> int32) | l -> P (innerParse l)
      (a', b')

    innerParse chars |> P

  let splitNumber number =
    match number with
    | L n -> P (L (n/2), L (n/2 + n % 2))
    | P _ -> failwith "Can't split a pair"

  let rec sprintSfNumber sfNumber =
    match sfNumber with
    | L literal -> sprintf $"{literal}"
    | P (e1, e2) -> sprintf $"[{sprintSfNumber e1},{sprintSfNumber e2}]"

  let psoSfNumber s sfn =
    let numberString = sprintSfNumber sfn
    printfn $"{s}{numberString}"
    sfn

  let rec something depth snf =
    match snf with
    | P (L lit1, L lit2) when depth = 4 -> Some (depth, snf)
    | P (snf1, snf2) ->
        [ something (depth + 1) snf1 ; something (depth + 1) snf2 ]
        |> List.choose id
        |> List.tryHead
    | L _ -> None

  type Direction = Left of SfNumber | Right of SfNumber

  let rec something2 acc depth snf =
    match snf with
    | P (L lit1, L lit2) when depth = 4 -> Some (acc |> List.rev, depth, snf)
    | P (snf1, snf2) ->

        [ something2 ((Left snf1)::acc) (depth + 1) snf1 ; something2 ((Right snf2)::acc) (depth + 1) snf2 ]
        |> List.choose id
        |> List.tryHead
    | L _ -> None



  module Part1 =
    let go () =
      //let e =
      //  getInput ()
      //  |> List.map parse
      //  |> pso "Pair list: "
      //  |> fun el ->
      //      let l = el |> List.map sprintNumber
      //      ps "As strings: " l
      //      el
      //
      //  |> ignore
      "[[[[[9,8],1],2],3],4]"
      |> Seq.toList
      |> parse
      |> psoSfNumber "Parsed: "
      |> something2 [] 0

  module Part2 =
    let go () =
      ()

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "

(*
[
  ([Left (P (P (P (P (L 9, L 8), L 1), L 2), L 3));
    Left (P (P (P (L 9, L 8), L 1), L 2)); Left (P (P (L 9, L 8), L 1));
    Left (P (L 9, L 8))], 4, P (L 9, L 8))


*)
