namespace AdventOfCode

open Helpers

module Day4 =
  open Microsoft.FSharp.Collections
  open FsToolkit.ErrorHandling

  module Common =
    let loadLines = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day4Input.txt") |> List.ofSeq

  module Part1 =
    type UnvalidatedPassport = List<string * string>

    let parseInput input : List<UnvalidatedPassport> =
      input
      |> List.splitMultipleOnExcl ((=) "") // split into passports
      |> List.map ( List.collect (String.splitMultipleOnCharExcl ' ') >> List.map (String.splitOnceOnChar ':') ) // map each passport into (key, value) strings

    let requiredFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

    let isPassportValid (passport : UnvalidatedPassport) =
      let fieldsInPassport = passport |> List.map (fun (fieldName, _) -> fieldName)
      let isFieldInPassport fieldName = List.contains fieldName fieldsInPassport
      requiredFields |> List.fold (fun acc fieldName -> acc && isFieldInPassport fieldName) true

  module Part2 =
    open Parsing
    open Part1

    let intIsInRangeInclusiveO min max i = if i >= min && i <= max then Some i else None

    let matchYear fieldName s min max =
      match FsRegEx.matches "^([0-9][0-9][0-9][0-9])$" s with
        | [|s|] -> s.Value |> parseIntO |> Option.bind (intIsInRangeInclusiveO min max)
        | _ -> None
      |> Result.requireSome (sprintf "Invalid year in %s" fieldName)

    type Distance = Centimetre of int | Inch of int

    let matchHeight s =
      FsRegEx.matches "(^([0-9][0-9])in$|^([0-9][0-9][0-9])cm$)" s
      |> Array.tryHead // only interested in the first match
      |> Option.bind (fun fsMatch ->
          match fsMatch.Groups() |> Array.map (fun group -> group.Value) with
          | [| _; _; inchValue; "" |] -> inchValue |> parseIntO |> Option.bind (intIsInRangeInclusiveO 59 76) |> Option.map Inch
          | [| _; _; ""; cmValue |]   -> cmValue   |> parseIntO |> Option.bind (intIsInRangeInclusiveO 150 193) |> Option.map Centimetre
          | _ -> None)
      |> Result.requireSome "Invalid height"

    let matchHairColour s =
      FsRegEx.matches "^#([0-9|a-f][0-9|a-f][0-9|a-f][0-9|a-f][0-9|a-f][0-9|a-f])$" s
      |> Array.tryHead
      |> Option.map (fun n -> n.Groups().[1].Value)
      |> Result.requireSome "Invalid hair colour"

    let matchPasswordId (s : string) =
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
      | Cid of string

    type ValidPassport = List<Property>

    let parseProperty (fieldName, valueString) =
      match fieldName with
      | "byr" -> matchYear "BirthDate" valueString 1920 2002 |> Result.map BirthYear
      | "iyr" -> matchYear "Issue year" valueString 2010 2020 |> Result.map IssueYear
      | "eyr" -> matchYear "Expiration year" valueString 2020 2030 |> Result.map ExpirationYear
      | "hgt" -> matchHeight valueString |> Result.map Height
      | "hcl" -> matchHairColour valueString |> Result.map HairColour
      | "ecl" -> matchEyeColour valueString |> Result.map EyeColour
      | "pid" -> matchPasswordId valueString |> Result.map PasswordId
      | "cid" -> Ok <| Cid valueString
      | _ -> Error "Unknown field"

    let parsePassport (passport : UnvalidatedPassport) : Validation<ValidPassport, string> =
      passport |> List.map parseProperty |> List.sequenceResultA

  module Run =
    let run () =
      Common.loadLines
      |> Part1.parseInput
      |> List.filter Part1.isPassportValid
      |> fun l -> l |> List.length |> printfn "Valid in part 1: %A passports"; l
      |> List.map Part2.parsePassport
      |> List.filter Validation.isOk
      |> List.length
      |> printfn "Valid in part 2: %A passports"
