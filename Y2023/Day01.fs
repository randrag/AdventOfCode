namespace AdventOfCode.Y2023

open Helpers

open NoCaml

module Day01 =

   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.map (Seq.map char)

         |> Seq.toList
         // filter for digits
         |> List.map Seq.toList
         |> List.map (List.filter (fun c ->
            (byte c >= byte '1') && (byte c <= byte '9')))
            |> List.map (fun digits ->
               //p digits
               let first = digits |> List.head |> int
               let last = digits |> List.last |> int

               //p (first, last)
               (first - 48) * 10 + last - 48

               )


      let go (year, day) runMode =
         let digits =
            getInput (year, day) runMode
            |> parse

         digits
         |> List.sum



   module Part2 =

      let digitStrings = [
         "one", 1; "two", 2; "three", 3; "four", 4; "five", 5; "six", 6; "seven", 7; "eight", 8; "nine", 9
         "1", 1; "2", 2; "3", 3; "4", 4; "5", 5; "6", 6; "7", 7; "8", 8; "9", 9
      ]

      // returns a list of all the positions in a string where the given substring is present
      let findStringInString (s : string) (subString : string) : List<int> = //List<int> =

         let rec loop (acc : List<int>) (index : int) =
            let s2 = s.Substring index
            match s2.StartsWith subString, index = s.Length with
            | true, true -> (index :: acc) |> List.rev
            | true, false -> loop (index :: acc) (index + 1)
            | false, true -> acc |> List.rev
            | false, false -> loop acc (index + 1)

         loop [] 0


      let go (year, day) runMode =

         let a =
            getInput (year, day) runMode
            |> Seq.toList
            |> List.map (fun inputStrings ->
                  digitStrings
                  |> List.collect (
                     fun (digitString, digit) ->
                        findStringInString inputStrings digitString
                        |> List.map (fun index -> (index, digit))
                        )
                     )
            |> id
         let earliestDigits =
            a
            |> List.map (List.sortBy fst)
            |> List.map (List.head)
            |> List.map snd
            |> id

         let latestDigits =
            a
            |> List.map (List.sortByDescending fst)
            |> List.map (List.head)
            |> List.map snd


         List.zip earliestDigits latestDigits
         |> List.map (fun (a, b) -> a*10 + b)
         |> List.sum






   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      RunMode.Full |> Part2.go (year, day) |> printn
      //54718


   module Test =
      open Xunit
      open Swensen.Unquote

      [<Fact>]
      let ``test 1`` () =
         let input = "1234"
         let expected = [2]
         let actual = Part2.findStringInString input "3"
         test <@ actual = expected @>

      [<Fact>]
      let ``test 2`` () =
         let input = "12two34"
         let expected = [2]
         let actual = Part2.findStringInString input "two"
         test <@ actual = expected @>

      [<Fact>]
      let ``test 3`` () =
         let input = "12twone"
         let expected = [4]
         let actual = Part2.findStringInString input "one"
         test <@ actual = expected @>
