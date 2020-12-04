module Day4
open Microsoft.FSharp.Collections
open FsToolkit.ErrorHandling
open System

module Common =
  let loadLines = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day4Input.txt") |> List.ofSeq

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
  let fromCharList (cl : char list) = cl |> Array.ofList |> String

  let splitOnceOnChar splitChar s =
    List.ofSeq s
    |> List.splitOnceOn ((=) splitChar)
    |> fun (a, b) -> fromCharList a, fromCharList b

  let splitMultipleOnChar splitChar (s : string) =
    List.ofSeq s
    |> List.splitMultipleOn ((=) splitChar)
    |> List.map fromCharList

module Part1 =
  type UnvalidatedPassport = List<string * string>

  let parseInput input : List<UnvalidatedPassport> =
    input
    |> List.splitMultipleOn ((=) "") // split into passports
    |> List.map (
         List.collect (String.splitMultipleOnChar ' ')
         >> List.map (String.splitOnceOnChar ':') // convert to (property, value) tuples
       )

  let allFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid"]
  let requiredFields = List.filter ((<>) "cid") allFields

  let isPassportValid (passport : UnvalidatedPassport) =
    let fieldsInPassport = passport |> List.map (fun (fieldName, _) -> fieldName)
    let isFieldInPassport fieldName = List.contains fieldName fieldsInPassport
    requiredFields |> List.fold (fun acc fieldName -> acc && isFieldInPassport fieldName) true

module Part2 =
  open TryParser
  open Part1

  let intIsInRangeInclusiveO min max i = if i >= min && i <= max then Some i else None

  let matchYear fieldName s min max =
    match FsRegEx.matches "^([0-9][0-9][0-9][0-9])$" s with
      | [|s|] ->
          s.Value
          |> parseIntO
          |> Option.bind (intIsInRangeInclusiveO min max)
      | _ -> None
    |> Result.requireSome (sprintf "Invalid year in %s" fieldName)

  type Distance = Centimetre of int | Inch of int


  let matchHeight s =
    FsRegEx.matches "(^([0-9][0-9])in$|^([0-9][0-9][0-9])cm$)" s
    |> Array.tryHead // only interested in the first match
    |> Option.map (fun n ->
        match n.Groups() |> Array.map (fun group -> group.Value) with
        | [| _; _; inchValue; "" |] ->
            inchValue
            |> parseIntO
            |> Option.bind (intIsInRangeInclusiveO 59 76)
            |> Option.map Inch
        | [| _; _; ""; cmValue |] ->
            cmValue
            |> parseIntO
            |> Option.bind (intIsInRangeInclusiveO 150 193)
            |> Option.map Centimetre
        | _ -> None)
        |> Option.flatten
        |> Result.requireSome "Invalid height"


  let matchHairColour s =
    FsRegEx.matches "^#([0-9|a-f][0-9|a-f][0-9|a-f][0-9|a-f][0-9|a-f][0-9|a-f])$" s
    |> Array.tryHead
    |> Option.map (fun n -> n.Groups().[1].Value)
    |> Result.requireSome "Invalid hair colour"

  let matchPasswordId (s : string) =
    let length = Seq.length s
    let padding = if length < 9 then String.replicate (9 - length) "0" else ""
    let paddedPasswordId = padding + s
    FsRegEx.matches "^([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9])$" s
    |> Array.tryHead
    |> Option.map (fun n -> n.Groups().[1].Value)
    |> Result.requireSome "Invalid password Id"

  let matchEyeColour s =
    match s with
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> Ok s
    | _ -> Error "Invalid eye colour"

  type Property =
    | BirthYear of int
    | IssueYear of int
    | ExpirationYear of int
    | Height of Distance
    | HairColour of string
    | EyeColour of string
    | PasswordId of string


  let parseProperty (fieldName, valueString) =
    match fieldName with
    | "byr" -> matchYear "BirthDate" valueString 1920 2002 |> Result.map BirthYear
    | "iyr" -> matchYear "Issue year" valueString 2010 2020 |> Result.map IssueYear
    | "eyr" -> matchYear "Expiration year" valueString 2020 2030 |> Result.map ExpirationYear
    | "hgt" -> matchHeight valueString |> Result.map Height
    | "hcl" -> matchHairColour valueString |> Result.map HairColour
    | "ecl" -> matchEyeColour valueString |> Result.map EyeColour
    | "pid" -> matchPasswordId valueString |> Result.map PasswordId

    | _ -> Error "Unknown field"

  let parsePassport (passport : UnvalidatedPassport) =
    passport
    |> List.map parseProperty
    |> List.sequenceResultA

  let isValidPassport = parsePassport >> Result.isOk

module Run =
  open Common
  open Part2
  let run () =

    loadLines
    |> Part1.parseInput
    |> List.filter Part1.isPassportValid
    |> List.length
    |> printfn "Valid in part 1: %A passports"

    (*
    loadLines
    |> Part1.parseInput
    |> List.map (fun passport -> passport, passport |> List.map (fun property -> property |> Part2.parseProperty))
    |> printfn "%A"
    *)

    loadLines
    |> Part1.parseInput
    |> List.filter Part1.isPassportValid
    |> List.map (fun i -> i, Part2.parsePassport i)
    |> List.map (fun (map, parsed) ->
          printfn "map : %A" map
          printfn "parsed : %A" parsed
          printfn "================"
          (map, parsed))
    |> List.filter (fun (_,b) -> b |> Result.isOk)
    |> List.length
    |> printfn "Valid in part 2: %A passports"
