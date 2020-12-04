namespace AdventOfCode

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


