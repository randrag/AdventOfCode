namespace AdventOfCode.Y2023

open Helpers

module Day04 =

   // Part 1
   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.map (fun line ->

            let cardString, numbers = String.splitOnceOnChar ':' line
            let cardNumber = cardString.Substring 4 |> int
            let winningNumbers =
               numbers
               |> String.splitOnceOnChar '|'
               |> fst
               |> String.splitMultipleOnCharExcl ' '
               |> List.map int


            let myNumbers =
               numbers
               |> String.splitOnceOnChar '|'
               |> snd
               |> String.splitMultipleOnCharExcl ' '
               |> List.map int


            cardNumber, winningNumbers, myNumbers

            )
         //|> pso "String list"

      let myWinningNumbers myNumbers winningNumbers =
         myNumbers
         |> List.filter (fun myNumber -> List.contains myNumber winningNumbers)

      let go (year, day) runMode =
         let inputL =
            getInput (year, day) runMode
            |> parse

         let myWinningNumbers =
            inputL
            |> List.map (fun (cardNumber, winningNumbers, myNumbers) ->
                  myWinningNumbers myNumbers winningNumbers
               )
            //|> pso "My Winning Numbers: "
            |> List.map (fun myWinningNumbers ->
                  myWinningNumbers |> List.length
               )

         let toPowerOf2 =
            myWinningNumbers
            |> List.map (fun countOfWinningNumbers ->
                  // have to raise to power of 2
                  let answer = pown 2 (countOfWinningNumbers - 1)
                  answer
                  )

         toPowerOf2 |> List.sum

         // filter myNumbers by winningNumbers



   // Part 2
   module Part2 =

      let countOfWinningNumbersByCardNumber inputL =
            inputL
            |> List.map (fun (cardNumber, winningNumbers, myNumbers) ->
                  let countOfWinningNumbers =
                     myNumbers
                     |> List.filter (fun myNumber -> List.contains myNumber winningNumbers)
                     |> List.length

                  cardNumber, countOfWinningNumbers
               )
            |> List.toMap
            |> pso "Count of Winning Numbers by Card Number: "


      let getNSubsequentCards startingCardNumber n (cards : Map<int, int>) =
         let numbersToGet = [(startingCardNumber + 1) .. (startingCardNumber + n)]
         let cards =
            numbersToGet
            |> List.map (fun cardNumber ->
                  cardNumber, cards.[cardNumber]
               )
         cards



      let go (year, day) runMode =
         let inputL =
            getInput (year, day) runMode
            |> Part1.parse

         let cardM = countOfWinningNumbersByCardNumber inputL

         // this is correct but too slow
         let rec doIt acc remaining =
            match remaining with
            | [] -> acc
            | (cardNumber, countOfWinningNumbers) :: remaining ->
               let drawn =
                  getNSubsequentCards cardNumber countOfWinningNumbers cardM

               let acc' = (cardNumber, countOfWinningNumbers) :: acc
               let remaining' = remaining @ drawn

               doIt acc' remaining'

         // have to do it in reverse order, and store the number of cards that are drawn so that we can
         // look them up instead of recomputing them
         let doIt2 (cardM : Map<int, int>) =

            let rec inner
               (processedCardM : Map<int, int>) // Map of card numbers and count of cards they pick up
               (remaining : List<int * int>)
               =
                  match remaining with
                  | [] -> processedCardM
                  | (cardNumber, count) :: cards ->
                     let PickedUpCards = getNSubsequentCards cardNumber count cardM
                     let countOfPickedUpCards = PickedUpCards.Length
                     let sumOfPickedUpCards =
                        PickedUpCards
                        |> List.map (fun (cardNumber, countOfCardsToPickUp) ->
                              


                              (countOfPickedUpCards) * (processedCardM[cardNumber] + countOfPickedUpCards)
                        )
                        |> List.sum
                     let map' = Map.add cardNumber sumOfPickedUpCards processedCardM
                     inner map' cards





            let cardL = cardM |> Map.toList |> List.sortByDescending fst
            inner Map.empty cardL


         //let x =
         //   getNSubsequentCards 1 4 cardM
         //   |> pso "Test: "

         //let pile = doIt [] (cardM |> Map.toList)

         //pile.Length
         let x = doIt2 cardM |> pso "CardM: "
         x



   // Run
   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Example
      |> Part2.go (year, day)
      |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

//      [<Fact>]
//      let test1 () =
//         let input = [""]
//         let expected = [""]
//         let actual = parse input
//         test <@ expected = actual @>
