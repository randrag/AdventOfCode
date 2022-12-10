namespace AdventOfCode.Y2022

open Helpers

module Day09 =

   type Dir = | L | R | U | D
   type Pos = ( int * int )
   type Move = {Dir : Dir; Count : int}

   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.map (fun s -> s.Split " ")
         |> List.map Array.toList
         |> List.map (fun l ->
            match l with
            | ["R"; nc] -> { Dir = R; Count = nc |> String.parseToIntX }
            | ["L"; nc] -> { Dir = L; Count = nc |> String.parseToIntX }
            | ["U"; nc] -> { Dir = U; Count = nc |> String.parseToIntX }
            | ["D"; nc] -> { Dir = D; Count = nc |> String.parseToIntX }
            | _ -> Unreachable ()
            )

      let stepPos dir (x,y ) =
         match dir with
         | U -> x, y + 1
         | D -> x, y - 1
         | L -> x - 1, y
         | R -> x + 1, y

      let stepHead (pos : Pos) (move : Move) =
         let newPos = stepPos move.Dir pos
         let nextMoveO =
            if move.Count > 1 then Some { move with Count = move.Count - 1 }
            else None
         (newPos, nextMoveO)

      let stepTail ((hx, hy) : Pos) ((tx, ty) : Pos) =
         printfn $"\nEntering steptail with \n headPos {(hx, hy)} \n tailPos {(tx, ty)}"
         let dx = hx - tx
         let dy = hy - ty
         printfn $" dx, dy: {(dx,dy)}"
         let mx, my =
            match dx, dy with

            // head right of tail
            |  2,0 -> 1,0
            // head left of tail
            | -2,0 -> -1,0

            // head above of tail
            | 0,  2 -> 0, 1
            // head below of tail
            | 0, -2 -> 0, -1

            // head RU of tail
            |  2, -1 ->  1, -1
            |  2,  1 ->  1,  1

            |  1, -2 ->  1, -1
            | -1, -2 -> -1, -1

            | -1,  2 -> -1,  1
            |  1,  2 ->  1,  1

            | -2, -1 -> -1, -1
            | -2,  1 -> -1,  1

            | _ -> 0,0

         tx + mx, ty + my

      let printGrid (mx, my) (headPos, tailPos) =
         printfn ""
         for y in [my .. -1 .. 0] do
            printfn ""
            for x in [0 .. mx] do
               printf (
                  if (x,y) = headPos then "H"
                  elif (x,y) = tailPos then "T"
                  else ".")

      let rec processMove (headPos : Pos, tailPos : Pos, tailPosL) (move : Move) =
         printGrid (10,10) (headPos, tailPos)
         let headPos', move'O = stepHead headPos move
         let tailPos' = stepTail headPos' tailPos
         printfn $"\nhead: {headPos'},tail: {(tailPos')}, move: {move'O}"
         let tailPosL' = tailPos'::tailPosL
         match move'O with
         | Some move'' -> processMove (headPos', tailPos', tailPosL') move''
         | None -> (headPos', tailPos', tailPosL')

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse
         |> List.fold processMove ( (0,0), (0,0), [(0,0)] )
         |> Tuple3.third
         |> List.distinct
         |> List.length

   module Part2 =

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse

   let run (year, day) =
      Full |> Part1.go (year, day) |> printn
      //Full |> Part2.go (year, day) |> printn
