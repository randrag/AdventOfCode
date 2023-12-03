namespace AdventOfCode.Y2023

open Helpers

module Day02 =

   module Part1 =

      let removeGameNumber (s : string) =
         s |> String.splitOnceOnChar ':'

      let splitSets (s : string) =
         s |> String.splitMultipleOnCharExcl ';'

      let splitDraw (s : string) =
         let n', colour = s.Trim() |> String.splitOnceOnChar ' '
         n' |> int, colour.Trim()

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.map (fun line ->
               let g1, rest = line |> removeGameNumber
               let sets = rest |> splitSets
               let gameNumber = g1.Substring 5 |> int

               let sets =
                  sets
                  |> List.map (fun set ->
                        let subsets =
                           set
                           |> String.splitMultipleOnCharExcl ','
                           |> List.map (fun s ->
                                 let n, colour = s |> splitDraw
                                 (colour, n)
                              )
                           |> List.toMap
                        subsets
                     )
               (gameNumber, sets)
            )

      let availableCubes =
         Map [
            "red", 12
            "green", 13
            "blue", 14
         ]

      let isPossible availableCubes drawnCubes =
         drawnCubes
         |> Map.fold
               (fun acc colour n ->
                  match acc, n <= Map.find colour availableCubes with
                  | false, _ -> false
                  | true, true -> true
                  | true, false -> false)
               true



      let go (year, day) runMode =
         let games =
            getInput (year, day) runMode
            |> parse
            |> pso "Games: "

         let gamesWithImpossibleSets =
            games
            |> List.map (fun (gameNumber, sets) ->
                  let impossibleSets =
                     sets
                     |> List.filter (fun set ->
                        set
                        |> isPossible availableCubes
                        |> not
                     )
                  gameNumber, impossibleSets
               )
            |> List.filter (fun (gameNumber, impossibleSets) ->
                  List.isEmpty impossibleSets |> not
                  )
            |> List.map fst
            |> pso "Impossible: "

         let possibleGames =
            games
            |> List.filter (fun (gameNumber, sets) ->
                  not (List.contains gameNumber gamesWithImpossibleSets)
               )
            |> List.map fst
            |> pso "Possible: "

         let answer = possibleGames |> List.sum

         answer





   module Part2 =

      let go (year, day) runMode =
         let games =
            getInput (year, day) runMode
            |> Part1.parse
            |> pso "Games: "

         let requiredCubesByGame =
            games
            |> List.map (fun (_gameNumber, sets) ->
                  sets
                  |> List.fold
                        (fun acc draws ->
                           draws
                           |> Map.fold
                                 (fun acc colour n ->
                                    let inAcc = Map.tryFind colour acc
                                    match inAcc with
                                    | None -> Map.add colour n acc
                                    | Some pm ->
                                       if n > pm then Map.add colour n acc
                                       else acc
                                 )
                                 acc
                        )
                        (Map.empty : Map<string, int>)
               )
            |> pso "Required: "

         let answer =
            requiredCubesByGame
            |> List.map Map.toList
            |> List.map (List.map snd)
            |> List.map (List.reduce (*))
            |> List.sum

         answer


   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn


   module Test =
      open Xunit
      open Swensen.Unquote

      [<Fact>]
      let ``Part 1 - Example``() =
         let expected = "Game 1"," 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
         let actual = Part1.removeGameNumber "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
         test <@ actual = expected @>

      [<Fact>]
      let ``Get sets`` () =
         let expected = [" 3 blue, 4 red"; " 1 red, 2 green, 6 blue"; " 2 green"]
         let actual = Part1.splitSets " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
         test <@ actual = expected @>

      [<Fact>]
      let ``Split draw`` () =
         let expected = 3, "blue"
         let actual = Part1.splitDraw " 3 blue"
         test <@ actual = expected @>

      [<Fact>]
      let ``Test map`` () =
         let availableCubes =
            Map [
            "red", 12
            "green", 13
            "blue", 14
            ]

         let drawnCubes =
            Map [
              "red", 11
              "green", 13
              "blue", 14
            ]


         let actual = Part1.isPossible availableCubes drawnCubes

         test <@ actual = true @>

      [<Fact>]
      let ``Test map 2`` () =
         let availableCubes =
            Map [
            "red", 12
            "green", 13
            "blue", 14
            ]

         let drawnCubes =
            Map [
              "red", 13
              "green", 13
              "blue", 14
            ]


         let actual = Part1.isPossible availableCubes drawnCubes

         test <@ actual = false @>
