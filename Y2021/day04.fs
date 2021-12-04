namespace AdventOfCode.Y2021

open Helpers

type Square = { Value : int32; IsMarked : bool }
type Row = List<Square>
type Board = List<Row>

module Day04 =
  let getInput () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input04.txt") |> Seq.toList

  let parse (input : List<string>) =
    let numbers = input |> List.head |> fun (s : string) -> s.Split(',') |> Seq.toList |> List.map int32
    let boards =
      input
      |> List.skip 2
      |> List.fold // add board numbers by counting empty lines
           (fun (boardNumber, l) s -> if s = "" then (boardNumber + 1, l) else (boardNumber, (boardNumber, s)::l) )
           (1, [])
      |> snd
      |> List.rev
      |> List.map (fun (boardNumber, s) -> // cast to int32
          (boardNumber
           , s.Split(' ')
             |> Array.toList
             |> List.filter (fun s -> s <> "")
             |> List.map (fun s -> {Value = s |> int32; IsMarked = false })))
      |> List.groupByAndMap fst snd // each board is now a list of lists
      |> List.map snd  : List<Board> // throw away board number

    (numbers, boards)

  let markNumberInBoards number (boards : List<Board>) =
    boards
    |> List.map (fun board ->
        board |> List.map (fun row ->
          row |> List.map (fun square ->
            if number = square.Value then { square with IsMarked = true } else square ) : Row) : Board)

  let isWinningRow (row : Row) = row |> List.filter (fun square -> not square.IsMarked) |> List.isEmpty

  let isWinningBoard (board : Board) =
    let hasWinningRow = board |> List.exists isWinningRow
    let hasWinningCol = board |> List.transpose |> List.exists isWinningRow
    hasWinningCol || hasWinningRow

  let sumUnmarkedSquares board =
    board |> List.concat |> List.filter (fun square -> not square.IsMarked) |> List.sumBy (fun square -> square.Value)

  module Part1 =
    let go () =
      let numbers, boards = getInput () |> parse

      let rec findWinningBoard boards numbers =
        let numberToMark = List.head numbers
        let newBoards = boards |> markNumberInBoards numberToMark
        match newBoards |> List.tryFind isWinningBoard with
        | Some winningBoard -> winningBoard, numberToMark
        | None -> findWinningBoard newBoards (List.tail numbers)

      let winningBoard, winningNumber = findWinningBoard boards numbers
      let sum = winningBoard |> sumUnmarkedSquares
      sum * winningNumber

  module Part2 =
    let go () =
      let numbers, boards = getInput () |> parse

      let rec findLastWinningBoard remainingBoards remainingNumbers =
         let numberToMark = List.head remainingNumbers
         let newBoards = markNumberInBoards numberToMark remainingBoards
         let newRemainingBoards = newBoards |> List.filter (fun board -> isWinningBoard board = false)
         let remainingBoardCount = List.length newRemainingBoards
         if remainingBoardCount = 0 then newBoards |> List.find isWinningBoard, numberToMark
         else findLastWinningBoard newRemainingBoards (List.tail remainingNumbers)

      let lastWinningBoard, lastNumber = findLastWinningBoard boards numbers
      let sum = sumUnmarkedSquares lastWinningBoard
      sum * lastNumber

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
