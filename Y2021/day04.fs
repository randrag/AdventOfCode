namespace AdventOfCode.Y2021

open Helpers

type Square = { Value : int32; IsMarked : bool }
type Row = seq<Square>
type Board = seq<Row>

module Day04 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input04.txt")

  let parse (input : seq<string>) =
    let numbers =
      input
      |> Seq.head
      |> fun (s : string) -> s.Split(',')
      |> Seq.map int32

    let boardList =
      input
      |> Seq.tail |> Seq.tail // remove first two lines
      |> Seq.fold // add board numbers by counting empty lines
           (fun (boardNumber, l) s ->
              if s = "" then (boardNumber + 1, l)
              else (boardNumber, (boardNumber, s)::l) )
           (1, [])
      |> snd
      |> Seq.rev
      |> Seq.map (fun (boardNumber, s) -> // cast to int32
          (boardNumber
           , s.Split(' ')
             |> Array.toSeq
             |> Seq.filter (fun s -> s <> "")
             |> Seq.map (fun s -> {Value = s |> int32; IsMarked = false })
             )
          )
      |> Seq.groupByAndMap fst snd // each board is now a list of lists
      |> Seq.map snd
      |> Seq.map (fun rows -> rows |> Seq.map (fun row -> row:Row) : Board)
      |> Seq.cache

    (numbers, boardList)


  let markNumberInRow number (row : Row) =
     seq {
       for square in row do
         if number = square.Value then yield {square with IsMarked = true}
         else yield square } : Row

  let markNumberInBoard number (board : Board) =
        seq { for row in board do (markNumberInRow number row ) } : Board

  let markNumberInBoards number (boardSeq : seq<Board>) =
    seq { for board in boardSeq do (markNumberInBoard number board) }

  let isWinningRow (row : Row) =
    row
    |> Seq.filter (fun row -> row.IsMarked = false)
    |> Seq.isEmpty

  let isWinningBoard (board : Board) =
    let hasWinningRow =
      board
      |> Seq.exists (fun row -> row |> isWinningRow = true)

    let hasWinningCol =
      board
      |> Seq.transpose
      |> Seq.exists (fun row -> row |> isWinningRow = true)

    hasWinningCol || hasWinningRow

  module Part1 =
    let go () =
      let numbers, boards = getInput () |> parse

      let rec findWinningBoard boards numbers prevNumberO =
          match boards |> Seq.tryFind isWinningBoard with
          | Some winningBoard -> winningBoard, prevNumberO |> Option.get
          | None ->
              printfn $"Marking {numbers |> Seq.head}"
              let numberToMark = Seq.head numbers
              let newBoards = markNumberInBoards numberToMark boards
              findWinningBoard newBoards (Seq.tail numbers) (Some numberToMark)

      let winningBoard, winningNumber = findWinningBoard boards numbers None

      let sum =
        winningBoard
        |> Seq.concat
        |> Seq.filter (fun square -> square.IsMarked = false)
        |> Seq.sumBy (fun square -> square.Value)

      sum * winningNumber

  module Part2 =
    let go () =
      printfn "\n\nGo2"
      let numbers, boards = getInput () |> parse

      let rec findLastWinningBoard remainingBoards remainingNumbers =
         let numberToMark = Seq.head remainingNumbers
         printfn $"Marking {numberToMark}"
         let newBoards = markNumberInBoards numberToMark remainingBoards
         let newRemainingBoards = newBoards |> Seq.filter (fun board -> isWinningBoard board = false)
         let remainingBoardCount = Seq.length newRemainingBoards |> pso "Remaining boards after marking: "
         if remainingBoardCount = 0 then
           newBoards |> Seq.head, numberToMark
         else findLastWinningBoard newRemainingBoards (Seq.tail remainingNumbers)

      let lastWinningBoard, lastNumber = findLastWinningBoard boards numbers

      let sum2 =
        lastWinningBoard
        |> Seq.concat
        |> Seq.filter (fun square -> square.IsMarked = false)
        |> Seq.sumBy (fun square -> square.Value)
        |> pso "sum2: "

      sum2 * lastNumber

  module Part2b =
    let go () =
      printfn "\n\nGo2"
      let numbers, boards = getInput () |> parse

      let rec findLastWinningBoard (remainingBoards : List<Board>) (remainingNumbers : List<int32>) =
         let numberToMark = List.head remainingNumbers
         printfn $"Marking {numberToMark}"
         let newBoards = markNumberInBoards numberToMark remainingBoards |> List.ofSeq
         let newRemainingBoards = newBoards |> List.filter (fun board -> isWinningBoard board = false)
         let remainingBoardCount = Seq.length newRemainingBoards |> pso "Remaining boards after marking: "
         if remainingBoardCount = 0 then
           newBoards |> List.head, numberToMark
         else findLastWinningBoard newRemainingBoards (List.tail remainingNumbers)

      let lastWinningBoard, lastNumber = findLastWinningBoard (boards |> Seq.toList) (numbers |> Seq.toList)

      let sum2 =
        lastWinningBoard
        |> Seq.concat
        |> Seq.filter (fun square -> square.IsMarked = false)
        |> Seq.sumBy (fun square -> square.Value)
        |> pso "sum2: "

      sum2 * lastNumber


  let run () =
    Part1.go () |> ps "Part 1: "
    Part2b.go () |> ps "Part 2: "
