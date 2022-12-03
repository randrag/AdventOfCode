namespace AdventOfCode.Y2022

open Helpers

type Play = | Rock | Paper | Scissors


module Day02 =

   let ABC = function
   | 'A' -> Rock
   | 'B' -> Paper
   | 'C' -> Scissors
   | _ -> Unreachable ()

   type Outcome = | Win | Lose | Tie

   let winLose a b =
      match a, b with
      | Rock, Rock -> Tie
      | Rock, Paper -> Lose
      | Rock, Scissors -> Win
      | Paper, Rock -> Win
      | Paper, Paper -> Tie
      | Paper, Scissors -> Lose
      | Scissors, Rock -> Lose
      | Scissors, Paper -> Win
      | Scissors, Scissors -> Tie

   let Score a b =
      let outcome = winLose b a
      let x =
         match b with
         | Rock -> 1
         | Paper -> 2
         | Scissors -> 3

      let y =
         match outcome with
         | Win -> 6
         | Tie -> 3
         | Lose -> 0

      x + y

   let getInput () =
      System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/input02.txt")
      |> Seq.toList


   module Part1 =

      let XYZ = function
      | 'X' -> Rock
      | 'Y' -> Paper
      | 'Z' -> Scissors
      | _ -> Unreachable ()

      let go () =
         getInput ()
         |> List.map (fun (s : string) ->
               let a = s[0] |> ABC
               let b = s[2] |> XYZ
               (a,b)
            )
         |> List.map (fun (a,b) -> Score a b)
         |> List.sum


   module Part2 =
      let XYZ opponentChoice xyz =
         match xyz with
         | 'X' -> // need to loose
            match opponentChoice with
            | Rock      -> Scissors
            | Paper     -> Rock
            | Scissors  -> Paper
         | 'Y' -> // need to draw
            match opponentChoice with
            | Rock      -> Rock
            | Paper     -> Paper
            | Scissors  -> Scissors
         | 'Z' -> // need to win
            match opponentChoice with
            | Rock      -> Paper
            | Paper     -> Scissors
            | Scissors  -> Rock
         | _ -> Unreachable ()

      let go () =
         getInput ()
         |> List.map (fun (s : string) ->
               let opponentChoice = s[0] |> ABC
               let myChoice = s[2] |> XYZ opponentChoice
               Score opponentChoice myChoice
            )
         |> List.sum

   let run () =
      Part1.go () |> print
      Part2.go () |> print
