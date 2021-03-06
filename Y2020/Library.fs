namespace AdventOfCode

[<AutoOpen>]
module OutputToConsole =
    open System

    let p = printfn
    let ps s x = printfn "%s%A" s x
    let pso s x = ps s x; x
    let pnl () =  printfn ""

    let r = Console.ReadKey () |> ignore
    let ro x = Console.ReadKey () |> ignore; x


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

  /// val windowed : windowSize:int -> list:'T list -> 'T list list
  let windowed = List.windowed : int -> List<'a> -> List<List<'a>>



  /// Applies a key-generating function to each element of a list and yields a list of unique keys. Each unique key contains a list of all elements that match to this key
  let groupBy2 = List.groupBy : ('a -> 'b) -> List<'a> -> List<('b * List<'a>)>
  let groupBy = List.groupBy : ('a -> 'b) -> List<'a> -> List<('b * List<'a>)>

  let filter = List.filter : ('a -> bool) -> List<'a> -> List<'a>

  let sort = List.sort : List<'a> -> List<'a>
  let concat = List.concat : seq<List<'a>> -> List<'a>
  let zip = List.zip : List<'a> -> List<'b> -> List<('a * 'b)>
  let take = List.take : int -> List<'a> -> List<'a>
  /// gives two lists back, excluding the splitting element
  let splitOnceOnExcl (f : 'a -> bool) l =
    let rec inner acc remaining =
      match remaining with
      | []                     -> (List.rev acc, [])
      | head::tail when f head -> (List.rev acc, tail)
      | head::tail             -> inner (head::acc) tail

    inner [] l

  /// Splits a list into multiple lists at each position where an element matches the predicate
  /// The element which matches the predicate is discarded
  let splitMultipleOnExcl (f : 'a -> bool) l : List<List<'a>> =
    let rec inner acc remaining =
      match remaining with
      | [] -> acc |> List.filter (List.isEmpty >> not) |> List.rev
      | something ->
          let (element, remaining) = splitOnceOnExcl f something
          inner (element::acc) remaining

    inner [[]] l



  /// Splits a list into two at the element where the predicate is first true.
  /// Element which causes true predicate is placed in second list
  /// Does not apply the predicate to the first element in the list - will not split to empty first list
  /// e.g. [1; 2; 3; 1; 2; 3; 1; 2] split on 1 becomes [1; 2; 3], [1; 2; 3; 1; 2]
  let splitOnceOnInclVx (isRear : 'a -> bool) l : List<'a> * List<'a> =
    let rec inner front' possiblyRear = // front' because it is reversed
      match possiblyRear with
      | []    ->                                     [],  List.rev front'
      | [x]   -> if isRear x then    front' |> List.rev, [x]
                             else x::front' |> List.rev, []
      | x::xs -> if isRear x then    front' |> List.rev, possiblyRear
                             else inner (x::front') xs
    inner [] l

  /// Splits a list into two at the element where the predicate is first true.
  /// Element which causes true predicate is placed in second list
  /// Does not apply the predicate to the first element in the list - will not split to empty first list
  /// e.g. [1; 2; 3; 1; 2; 3; 1; 2] split on 1 becomes [1; 2; 3], [1; 2; 3; 1; 2]
  let splitOnceOnIncl (isRear : 'a -> bool) l : List<'a> * List<'a> =
    let rec inner front' possiblyRear = // front' because it is reversed
      match possiblyRear with
      | []    ->                                     [],  List.rev front'
      | [x]   -> if isRear x then    front' |> List.rev, [x]
                             else x::front' |> List.rev, []
      | x::xs -> if isRear x then    front' |> List.rev, possiblyRear
                             else inner (x::front') xs

    match l with
    | []  -> [] , []
    | [x] -> [x], []
    | x::xs -> inner [] xs |> fun (front, rear) -> x::front, rear


  let private testSplitOnceOnExcl () =
      []           |> splitOnceOnIncl ((=) 1) = ([] ,          []          ) |> ps "True?: "
      [1]          |> splitOnceOnIncl ((=) 2) = ([1],          []          ) |> ps "True?: "
      [2]          |> splitOnceOnIncl ((=) 2) = ([2],          []          ) |> ps "True?: "
      [1; 2]       |> splitOnceOnIncl ((=) 2) = ([1],          [2]         ) |> ps "True?: "
      [1; 2; 2; 3] |> splitOnceOnIncl ((=) 2) = ([1],          [2; 2; 3]   ) |> ps "True?: "
      [1; 2; 2; 3] |> splitOnceOnIncl ((=) 4) = ([1; 2; 2; 3], []          ) |> ps "True?: "
      [1; 2; 2; 3] |> splitOnceOnIncl ((=) 1) = ([1; 2; 2; 3], []          ) |> ps "True?: "
      [1; 2; 1; 2] |> splitOnceOnIncl ((=) 1) = ([1; 2],       [1; 2]      ) |> ps "True?: "

  /// Splits a list into multiple lists at each position where an element matches the predicate
  /// The element which matches the predicate is included as the head of the list after each split
  let splitMultipleOnIncl (f : 'a -> bool) l : List<List<'a>> =
    let rec inner acc remaining =
      match remaining with
      | [] -> acc |> List.filter (List.isEmpty >> not) |> List.rev
      | something ->
          let (front, remaining) = splitOnceOnIncl f something
          inner (front::acc) remaining

    inner [[]] l
(*
      []           |> List.splitWhere ((=) 1) = ([] ,          []          ) |> ps "Test splitWhere: "
      [1]          |> List.splitWhere ((=) 2) = ([1],          []          ) |> ps "Test splitwhere: "
      [2]          |> List.splitWhere ((=) 2) = ([] ,          [2]         ) |> ps "Test splitwhere: "
      [1; 2]       |> List.splitWhere ((=) 2) = ([1],          [2]         ) |> ps "Test splitwhere: "
      [1; 2; 2; 3] |> List.splitWhere ((=) 2) = ([1],          [2; 2; 3]   ) |> ps "Test splitwhere: "
      [1; 2; 2; 3] |> List.splitWhere ((=) 4) = ([1; 2; 2; 3], []          ) |> ps "Test splitwhere: "
      [1; 2; 2; 3] |> List.splitWhere ((=) 1) = ([],           [1; 2; 2; 3]) |> ps "Test splitwhere: "
*)

module String =
  open System
  let fromCharList (cl : char list) = cl |> Array.ofList |> String
  let fromCharSeq (cs : seq<char>) = cs |> Array.ofSeq |> String


  let splitOnceOnChar splitChar s =
    List.ofSeq s
    |> List.splitOnceOnExcl ((=) splitChar)
    |> fun (a, b) -> fromCharList a, fromCharList b

  let splitMultipleOnChar splitChar (s : string) =
    List.ofSeq s
    |> List.splitMultipleOnExcl ((=) splitChar)
    |> List.map fromCharList


module Validation =
  open FsToolkit.ErrorHandling
  let isOk = Result.isOk

[<AutoOpen>]
module LeftPipe =

    // I want the Haskell $ operator
    // To associate right it must start with ** or ^

    let ( **<| ) = (<|)
    let ( ^<| ) = (<|)

