namespace AdventOfCode

[<AutoOpen>]
module OutputToConsole =
    let p s = printfn s
    let ps s x = printfn "%s%A" s x
    let pso s x = ps s x; x
    let nl = printfn ""


// from fssnip.net
module TryParser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith (tryParseFunc: string -> bool * _) = tryParseFunc >> function
        | true, v    -> Some v
        | false, _   -> None

    let parseDateO   = tryParseWith System.DateTime.TryParse
    let parseIntO    = tryParseWith System.Int32.TryParse
    let parseSingleO = tryParseWith System.Single.TryParse
    let parseDoubleO = tryParseWith System.Double.TryParse
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|)   = parseDateO
    let (|Int|_|)    = parseIntO
    let (|Single|_|) = parseSingleO
    let (|Double|_|) = parseDoubleO

module List =
  /// shadow built in functions to get signatures correct
  let ofSeq = List.ofSeq : seq<'a> -> List<'a>
  let map = List.map : ('a -> 'b) -> List<'a> -> List<'b>

  ///Apply a function to each element of the collection, threading an accumulator argument through the computation.
  /// Apply the function to the first two elements of the list.
  /// Then feed this result into the function along with the third element and so on. Return the final result.
  /// If the input function is f and the elements are i0...iN then computes f (... (f i0 i1) i2 ...) iN.
  ///
  /// Full name: Microsoft.FSharp.Collections.List.reduce
  let reduce = List.reduce : ('a -> 'a -> 'a) -> List<'a> -> 'a

  /// gives two lists back, excluding the splitting element
  let splitOnceOn (f : 'a -> bool) l =
    let rec inner acc remaining =
      match remaining with
      | []                     -> (List.rev acc, [])
      | head::tail when f head -> (List.rev acc, tail)
      | head::tail             -> inner (head::acc) tail

    inner [] l

  let splitMultipleOn (f : 'a -> bool) l : List<List<'a>> =
    let rec inner acc remaining =
      match remaining with
      | [] -> acc |> List.filter (List.isEmpty >> not) |> List.rev
      | something ->
          let (element, remaining) = splitOnceOn f something
          inner (element::acc) remaining

    inner [[]] l

module String =
  open System
  let fromCharList (cl : char list) = cl |> Array.ofList |> String

  let splitOnceOnChar splitChar s =
    List.ofSeq s
    |> List.splitOnceOn ((=) splitChar)
    |> fun (a, b) -> fromCharList a, fromCharList b

  let splitMultipleOnChar splitChar (s : string) =
    List.ofSeq s
    |> List.splitMultipleOn ((=) splitChar)
    |> List.map fromCharList


module Validation =
  open FsToolkit.ErrorHandling
  let isOk = Result.isOk

[<AutoOpen>]
module LeftPipe =

    // I want the Haskell $ operator
    // To associate right it must start with ** or ^

    let ( **< ) = (<|)
    let ( *** ) = (<|)
    let ( ^< ) = (<|)
    let ( ^^ ) = (<|)
    let ( ^<| ) = (<|)
    let ( **<| ) = (<|)

    let add = (+)
    let mul = (*)

//    add 1  ^< add 2  ^< mul 4  ^< add 3 4 |> ps "1 " // 7 * 4 + 2 + 1 = 31
//    add 1 **< add 2 **< mul 4 **< add 3 4 |> ps "2 " // 7 * 4 + 2 + 1 = 31
//    add 1 *** add 2 *** mul 4 *** add 3 4 |> ps "3 " // 7 * 4 + 2 + 1 = 31
//
    // 2 + (3 + 4) * 5 + 2 = 39
//    2 |> add **<| add 2 **<| mul 5 **<| add 3 4 |> ps "3 "
//    2 |> add *** add 2 *** mul 5 *** add 3 4 |> ps "3 "
//    2 |> add ^<| add 2 ^<| mul 5 ^<| add 3 4 |> ps "best? "
//    2 |> add ^^ add 2 ^^ mul 5 ^^ add 3 4 |> ps "3 "

