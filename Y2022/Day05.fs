namespace AdventOfCode.Y2022

open Helpers



module Day05 =

   type Move = {
      Qty : int
      From : int
      To : int
   }

   module Part1 =

      let parse (lines : seq<string>) =
         let stacks, moves =
            lines
            |> Seq.toList
            |> List.splitOnceOnExcl (fun s -> s = "")

         let stackCount =
            stacks
            |> pso "Stacks"
            |> List.rev
            |> List.head // count
            |> fun s -> s.Split ' '
            |> Array.toList
            |> List.filter (fun s -> s <> "")
            |> List.map String.parseToIntX
            |> List.max
            |> pso "Max: "

         let positions =
            [1 .. stackCount]
            |> List.map (fun i -> (i - 1) * 4 + 1)
            |> pso "Positions: "

         let stack =
            stacks
            |> List.rev
            |> List.tail
            |> pso "Stack: "

         // get layers
         let x =
            [ for layer in stack do
               [ for position in positions do
                  ps "layer as array " (layer |> Seq.toArray)
                  ps "position " position

                  if position < layer.Length then
                     let letter = (layer |> Seq.toArray)[position]
                     yield if letter = ' ' then None else Some letter
                  else
                     yield None
               ]
            ]
         ps "X: " x

         let stacks =
            List.transpose x
            |> List.map (List.choose id)
            |> pso "Stacks: "
            |> List.map List.rev
            |> List.mapi (fun i stack -> (i + 1, stack))
            |> List.toMap
            |> pso "Stacks as map "


         let moves =
            moves
            |> pso "Moves incoming\n"
            |> List.map (fun s -> s.Split "move ")
            |> List.map (Array.collect (fun s -> s.Split " from "))
            |> List.map (Array.collect (fun s -> s.Split " to "))
            |> List.map (Array.toList)
            |> List.map (List.filter (fun s -> s <> ""))
            |> List.map (
                  fun sL ->
                  sL
                  |> List.map String.parseToIntX
                  |> function
                     | [qty; from; to'] -> {Qty = qty; From = from; To = to'}
                     | _ -> Unreachable ()

                  )

            |> pso "Moves:\n"

         (stacks, moves)


      let pop (l : List<char>) = (List.head l, List.tail l)
      let push c l = c::l

      let moveCrate (stacks : Map<int, List<char>>, fromStackNumber : int, toStackNumber : int) =
         let c, fromStack' = stacks[fromStackNumber] |> pop
         let toStack' = stacks[toStackNumber] |> push c

         stacks
         |> Map.add fromStackNumber fromStack'
         |> Map.add toStackNumber toStack'

      let doMove (stacks : Map<int, List<char>>) (move : Move) =
         let rec inner stacks move =
            if (move.Qty > 0) then
               let stacks' = moveCrate (stacks, move.From, move.To)
               let move' = {move with Qty = move.Qty - 1}
               inner stacks' move'
            else stacks
         inner stacks move
         |> pso "\n Moved: \n"

      let doMoves (stacks : Map<int, List<char>>) (moves : List<Move>) =
         moves
         |> List.fold doMove stacks


      let go (year, day) runMode =
         let stacks, moves =
            getInput (year, day) runMode
            |> parse

         doMoves stacks moves
         |> pso "Final stacks: "
         |> Map.toList
         |> List.map snd
         |> List.map List.head
         |> pso "Output: "
         |> List.toSeq
         |> Seq.reduce (+)


   module Part2 =

      let doMove (stacks : Map<int, List<char>>) (move : Move) =
         let movedCrates =
            stacks[move.From]
            |> List.take move.Qty
         let fromStack' =
            stacks[move.From]
            |> List.skip move.Qty
         let toStack' = List.concat [movedCrates ; stacks[move.To]]

         stacks
         |> Map.add move.From fromStack'
         |> Map.add move.To toStack'

      let doMoves (stacks : Map<int, List<char>>) (moves : List<Move>) =
         moves
         |> List.fold doMove stacks


      let go (year, day) runMode =
         let stacks, moves =
            getInput (year, day) runMode
            |> Part1.parse

         doMoves stacks moves
         |> pso "Final stacks: "
         |> Map.toList
         |> List.map snd
         |> List.map List.head
         |> pso "Output: "
         |> List.toSeq
         |> Seq.reduce (+)



   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
