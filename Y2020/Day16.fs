namespace AdventOfCode

open Helpers

module Day16 =
  let input ()  = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input16.txt") |> List.ofSeq

  let splitInputSections stringList =
    let fields = stringList |> List.takeWhile ( (<>) "")
    let yourTicket = stringList |> List.skipWhile ((<>) "your ticket:") |> List.tail |> List.head
    let nearbyTickets = stringList |> List.skipWhile ((<>) "nearby tickets:") |> List.skip 1
    (fields, yourTicket, nearbyTickets)

  let parseTicket (s : string) =
    s |> String.splitMultipleOnCharExcl ',' |> List.map int

  let parseRule (s : string) =
    let name = s.Substring (0, (s.IndexOf ':') )
    let fields = s.Substring (s.IndexOf ": " + 2)
    let allowedRanges =
      fields
      |> String.splitMultipleOnCharExcl ' '
      |> List.filter (fun s -> s <> "or")
      |> List.map (String.splitOnceOnChar '-')
      |> List.map (fun (a,b) -> int a, int b)

    name, allowedRanges

  let isInRange value range  =
    let (min : int, max : int) = range
    min <= value && value <= max

  let isValidForAtLeastOneRange allValidRanges value =
    List.fold (fun acc range -> acc || (isInRange value range) ) false allValidRanges

  let isInvalidForAllRanges allValidRanges value  =
    isValidForAtLeastOneRange allValidRanges value |> not

  let areAllValuesValid allValidRanges  ticket =
    // none of the values in the ticket is invalid for all ranges
    ticket
    |> List.filter (isInvalidForAllRanges allValidRanges)
    |> List.isEmpty

  /// transpose a list of equal length lists
  let transpose ( ll : List<List<'a>> ) : List<List<'a>> =
    let rec inner (remainingLL : List<List<'a>>) wip =
      if List.isEmpty remainingLL.[0]
      then wip |> List.rev |> List.tail
      else
        let newRow = remainingLL |> List.map List.head
        let remainingLists = remainingLL |> List.map List.tail
        inner remainingLists (newRow :: wip)
    inner ll [[]]



  let run () =
    let (fields, yourTicketString, nearbyTicketStrings) = input () |> splitInputSections

    let rules = fields |> List.map parseRule
    let myTicket = parseTicket yourTicketString
    let nearbyTickets = nearbyTicketStrings |> List.map parseTicket

    let allValidRanges =
      rules
      |> List.map (fun (_, ranges) -> ranges)
      |> List.concat

    do
      nearbyTickets
      |> List.concat
      |> List.filter (isInvalidForAllRanges allValidRanges)
      |> List.sum
      |> pso "Part 1 answer: "
      |> ignore

    // Part 2

    // find valid
    let validNearbyTickets =
      nearbyTickets
      |> List.filter (areAllValuesValid allValidRanges)
      |> pso "All valid tickets: "

    // transposed so that we group the values by potential field
    let ticketValuesByField =
      validNearbyTickets
      |> transpose
      |> pso "Transposed: "

    let x : List<List<int * string * bool>> =
      ticketValuesByField
      |> List.mapi (fun fieldNumber values ->
          rules |> List.map (fun (ruleName, ranges) ->
            (fieldNumber + 1, ruleName, areAllValuesValid ranges values) ))
      |> pso "Show me: \n"

    let fieldsWithPossibleColumns =
      x
      |> transpose
      |> pso "Show me transposed: \n"
      |> List.map (fun l ->
          l
          |> List.groupBy (fun (_, fieldName, _) -> fieldName)
          |> List.map (fun (fieldName, l) -> (fieldName, l |> List.map (fun (a,_,c) -> (a,c)) )
          ))
      |> pso "grouped: "
      |> List.concat
      |> List.map (fun (fieldName, l) -> fieldName, l |> List.filter snd)
      |> List.map (fun (fieldName, l) -> fieldName, l |> List.map (fun (fieldNumber, _) -> fieldNumber))
      |> pso "filtered: \n"

    let rec myReduce acc (fieldsWithPossibleColumns : List<string * List<int>>) =
      ps "\n" ""
      ps "fieldsWithPossibleColumns: \n" fieldsWithPossibleColumns
      ps "acc: \n" acc

      if List.isEmpty fieldsWithPossibleColumns then acc
      else
        let (assignedFieldName, assignedColumn) =
          fieldsWithPossibleColumns
          |> List.filter (fun (_, possibleColumns) -> List.length possibleColumns = 1)
          |> List.map (fun (s, il) -> s, List.head il)
          |> List.head

        let remainingFieldsWithPossibleColumns =
          fieldsWithPossibleColumns
          |> List.filter (fun (fieldName, _) -> fieldName <> assignedFieldName)
          |> List.map (fun (fieldName, columnList) ->
              fieldName
              , columnList
                |> List.filter ( (<>) assignedColumn ))

        myReduce ((assignedColumn, assignedFieldName )::acc) remainingFieldsWithPossibleColumns

    let flip f a b = f b a

    let fieldsWithColumns =
      myReduce [] fieldsWithPossibleColumns
      |> List.sortBy fst
      |> pso "Fields with possible columns: "
      |> List.map snd
      |> flip List.zip myTicket
      |> pso "my ticket: "
      |> List.filter (fun (fieldName, _) -> fieldName.StartsWith "departure")
      |> pso "my ticket filtered: "
      |> List.map snd
      |> List.map int64
      |> List.reduce (*)
      |> ps "Let's go!: "



    // now we want to check each value list against the allowed range for each field


    /// Now work out which field is which
    /// So for each position in the list of values of each ticket, you must figure out for which field they are all valid
    /// I think that you want to transpose the list of lists
    ///

    ()
