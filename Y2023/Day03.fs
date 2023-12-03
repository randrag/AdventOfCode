namespace AdventOfCode.Y2023

open Helpers

module Day03 =

   // Part 1
   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList


      type Coordinate = { x : int; y : int }
      type Number = { Value : int; PosL : List<Coordinate> }
      type Symbol = { Value : char; Pos : Coordinate }
      type Contents = | NumberC of Number | SymbolC of Symbol



      let parseLine (line : string) (startingPos : Coordinate) : List<Contents> =

         let rec inner
            (acc : List<Contents>)
            pos
            (partialNumberO : Option<int * List<Coordinate>>)
            (remaining : List<char>) =
               match remaining with
               | [] -> match partialNumberO with
                       | None -> acc
                       | Some (n, pl) -> NumberC { Value = n; PosL = pl } :: acc
               | head :: tail ->
                  match head with
                  | '.' ->
                     match partialNumberO with
                     | None -> inner acc { pos with x = pos.x + 1 } None tail
                     | Some (n, pl) ->
                        inner
                           ( NumberC { Value = n; PosL = pl } :: acc )
                           { pos with x = pos.x + 1 }
                           None
                           tail
                  |  '#'| '$'| '%'| '&'| '*'| '+'| '-'| '.'| '/'| '=' | '&' | '@' ->
                     match partialNumberO with
                     | None ->
                        inner
                           (SymbolC { Value = head; Pos = pos } :: acc)
                           { pos with x = pos.x + 1 }
                           None
                           tail
                     | Some (n, pl) ->
                        inner (SymbolC { Value = head; Pos = pos } :: NumberC { Value = n; PosL = pl } :: acc)
                           { pos with x = pos.x + 1 }
                           None
                           tail
                  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  as c ->
                     let n1 = int c - 48
                     match partialNumberO with
                     | None -> inner acc { pos with x = pos.x + 1 } (Some (n1, [pos])) tail
                     | Some (n, pl) ->
                        inner
                           acc
                           { pos with x = pos.x + 1 }
                           (Some (n * 10 + n1, pos :: pl))
                           tail

                  | _ -> failwithf "Unexpected character: %c" head

         let charL = String.toCharList line

         inner [] startingPos None charL

      let findDistinctCharsInString (s : string) =
         s
         |> String.toCharList
         |> List.distinct

      let areAdjacent (c1 : Coordinate) (c2 : Coordinate) =
         abs (c1.x - c2.x) <= 1 && abs (c1.y - c2.y) <= 1


      let getSymbols (contentsL : List<Contents>) =
         contentsL
         |> List.map (function | SymbolC s -> Some s | _ -> None)
         |> List.choose id

      let getNumbers (contentsL : List<Contents>) =
         contentsL
         |> List.map (function | NumberC n -> Some n | _ -> None)
         |> List.choose id

      let isAdjacentToSymbol (n : Number) (sL : List<Symbol>) =
         n.PosL
         |> List.exists (fun numberPos -> sL |> List.exists (fun symbol -> areAdjacent numberPos symbol.Pos))

      let go (year, day) runMode =

         let contents =
            let stringL =
               getInput (year, day) runMode
               |> Seq.toList

            stringL
            |> List.mapi (fun y l -> parseLine l { x = 0; y = y })
            |> List.concat

         let numbers = getNumbers contents
         let symbols = getSymbols contents

         let numbersAdjacentToSymbol = numbers |> List.filter (fun n -> isAdjacentToSymbol n symbols)

         numbersAdjacentToSymbol
         |> List.sumBy (fun n -> n.Value)



   // Part 2
   module Part2 =
      open Part1
      let go (year, day) runMode =
         let contents =
            let stringL =
               getInput (year, day) runMode
               |> Seq.toList

            stringL
            |> List.mapi (fun y l -> parseLine l { x = 0; y = y })
            |> List.concat

         let numbers = getNumbers contents
         let symbols = getSymbols contents

         let multiplySymbols =
            symbols
            |> List.filter (fun s -> s.Value = '*')

         let numbersAdjacentToMultiplySymbol =
            multiplySymbols
            |> List.map (fun s -> numbers |> List.filter (fun n -> isAdjacentToSymbol n [s]))
            |> id

         let onlyTwo =
            numbersAdjacentToMultiplySymbol
            |> List.filter (fun nL -> nL.Length = 2)

         let answer =
            onlyTwo
            |> List.map (fun nL -> nL |> List.map (fun n -> n.Value) |> List.reduce (*))
            |> List.sum

         answer




   // Run
   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

      [<Fact>]
      let t1 () =
         let input = "467......."
         let expected = [
            NumberC { Value = 467; PosL = [{ x = 2; y = 0 }; { x = 1; y = 0 }; { x = 0; y = 0 }] }
         ]
         let actual = parseLine input { x = 0; y = 0 }
         test <@ actual = expected @>

      [<Fact>]
      let t2 () =
         let input = "...467...."
         let expected = [
            NumberC { Value = 467; PosL = [{ x = 5; y = 0 }; { x = 4; y = 0 }; { x = 3; y = 0 }] }
         ]
         let actual = parseLine input { x = 0; y = 0 }
         test <@ actual = expected @>

      [<Fact>]
      let t3 () =
         let input = ".......467"
         let expected = [
            NumberC { Value = 467; PosL = [{ x = 9; y = 0 }; { x = 8; y = 0 }; { x = 7; y = 0 }] }
         ]
         let actual = parseLine input { x = 0; y = 0 }
         test <@ actual = expected @>

      [<Fact>]
      let t7 () =
         let input = ".123*504."
         let expected = [
            NumberC { Value = 504; PosL = [{ x = 7; y = 0 }; { x = 6; y = 0 }; { x = 5; y = 0 }] }
            SymbolC { Value = '*'; Pos = { x = 4; y = 0 } }
            NumberC { Value = 123; PosL = [ {x = 3; y = 0};  { x = 2; y = 0 }; { x = 1; y = 0 }] }
         ]
         let actual = parseLine input { x = 0; y = 0 }
         test <@ actual = expected @>

      [<Fact>]
      let t8 () =
         let input = "*123.504#"
         let expected = [
            SymbolC { Value = '#'; Pos = { x = 8; y = 0 } }
            NumberC { Value = 504; PosL = [{ x = 7; y = 0 }; { x = 6; y = 0 }; { x = 5; y = 0 }] }
            NumberC { Value = 123; PosL = [ {x = 3; y = 0};  { x = 2; y = 0 }; { x = 1; y = 0 }] }
            SymbolC { Value = '*'; Pos = { x = 0; y = 0 } }
         ]
         let actual = parseLine input { x = 0; y = 0 }
         test <@ actual = expected @>


      [<Fact>]
      let t4 () =
         let input = "..$......."
         let expected = [
            SymbolC { Value = '$'; Pos = { x = 2; y = 0 } }
         ]
         let actual = parseLine input { x = 0; y = 0 }
         test <@ actual = expected @>


      [<Fact>]
      let t5 () =
         let actual = areAdjacent { x = 0; y = 0 } { x = 1; y = 0 }
         let expected = true

         test <@ actual = expected @>

      [<Fact>]
      let t6 () =
         let actual = areAdjacent { x = 0; y = 0 } { x = 1; y = 1 }
         let expected = true

         test <@ actual = expected @>

      [<Fact>]
      let t9 () =
         let actual = areAdjacent { x = 0; y = 0 } { x = 2; y = 1 }
         let expected = false

         test <@ actual = expected @>
