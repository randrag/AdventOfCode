namespace AdventOfCode.Y2022

open Helpers



module Day05 =

   type Move = { Qty : int; FromStackNumber : int; ToStackNumber : int }

   module Part1 =

      let parse (lines : seq<string>) =
         let stacksInput, movesInput =
            lines
            |> Seq.toList
            |> List.splitOnceOnExcl (fun s -> s = "")

         let stackCount =
            stacksInput
            |> List.rev
            |> List.head // count
            |> fun s -> s.Split ' '
            |> Array.toList
            |> List.filter (fun s -> s <> "")
            |> List.map String.parseToIntX
            |> List.max

         let positions =  [1 .. stackCount] |> List.map (fun i -> (i - 1) * 4 + 1)

         let heap =
            stacksInput
            |> List.rev
            |> List.tail // remove numbers, each list is a line of the stack, bottom row first

         let layers =
            [ for layer in heap do
               [ for position in positions do
                  if position < layer.Length then
                     let letter = (layer |> Seq.toArray)[position]
                     yield if letter = ' ' then None else Some letter
                  else
                     yield None
               ]
            ]

         let parsedStacks =
            layers
            |> List.transpose  // now each list is a stack
            |> List.map (List.choose id >> List.rev)
            |> List.mapi (fun i stack -> (i + 1, stack))
            |> List.toMap

         let parsedMoves =
            movesInput
            |> List.map (fun s ->
                  s.Split "move "
                  |> Array.collect (fun s -> s.Split " from ")
                  |> Array.collect (fun s -> s.Split " to ")
                  |> Array.toList
                  |> List.filter (fun s -> s <> "")
                  |> List.map String.parseToIntX
                  |> function
                     | [qty; from; to'] -> {Qty = qty; FromStackNumber = from; ToStackNumber = to'}
                     | _ -> Unreachable ()
               )

         (parsedStacks, parsedMoves)

      let pop (l : List<char>) = (List.head l, List.tail l)
      let push c l = c::l

      let moveCrate (stacks : Map<int, List<char>>, fromStackNumber : int, toStackNumber : int) =
         let c, fromStack' = stacks[fromStackNumber] |> pop
         let toStack' = stacks[toStackNumber] |> push c

         stacks
         |> Map.add fromStackNumber fromStack'
         |> Map.add toStackNumber toStack'

      let doMove (stacks : Map<int, List<char>>) (move : Move)
         =
         let rec inner stacks move =
            if move.Qty > 0 then
               let stacks' = moveCrate (stacks, move.FromStackNumber, move.ToStackNumber)
               let move' = {move with Qty = move.Qty - 1}
               inner stacks' move'
            else stacks

         inner stacks move

      let doMoves (stacks : Map<int, List<char>>) (moves : List<Move>) =
         moves |> List.fold doMove stacks


      let go (year, day) runMode =
         let stacks, moves = getInput (year, day) runMode |> parse

         doMoves stacks moves
         |> Map.toList
         |> List.map snd
         |> List.map List.head
         |> System.String.Concat

   module Part2 =

      let doMove (stacks : Map<int, List<char>>) (move : Move) =
         let movedCrates = stacks[move.FromStackNumber] |> List.take move.Qty
         let fromStack' = stacks[move.FromStackNumber] |> List.skip move.Qty
         let toStack' = List.concat [movedCrates ; stacks[move.ToStackNumber]]

         stacks
         |> Map.add move.FromStackNumber fromStack'
         |> Map.add move.ToStackNumber toStack'

      let doMoves (stacks : Map<int, List<char>>) (moves : List<Move>)
         = List.fold doMove stacks moves

      let go (year, day) runMode =
         let stacks, moves =
            getInput (year, day) runMode
            |> Part1.parse

         doMoves stacks moves
         |> Map.toList
         |> List.map snd
         |> List.map List.head
         |> System.String.Concat

   let run (year, day) =
      Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
