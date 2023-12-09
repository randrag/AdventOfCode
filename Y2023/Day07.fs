namespace AdventOfCode.Y2023

open Helpers

module Day07 =

   // Part 1
   module Part1 =



      type Hand =
         Hand of  List<char>
         with member this.toCharL = match this with | Hand l -> l


      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.map (fun line ->
               let hand, bid =
                  line
                  |> String.splitOnceOnChar ' '
                  |> fun (handString, bidString) ->
                     handString |> String.toCharList |> Hand, bidString |> int64
               hand, bid
               )

      let compareCards (card1 : char) (card2 : char) =
         let cardOrder = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']
         let card1Index = cardOrder |> List.findIndex (fun c -> c = card1)
         let card2Index = cardOrder |> List.findIndex (fun c -> c = card2)
         card1Index.CompareTo card2Index



      let sortHand (hand : Hand) =
         hand.toCharL |> List.sortWith compareCards |> Hand

      let groupAndCount (hand : Hand) =
         hand.toCharL |> List.groupBy id |> List.map (fun (c, l) -> c, l.Length)

      type TypedHand =
         | FiveOfAKind   of List<char>
         | FourOfAKind   of List<char>
         | ThreeOfAKind  of List<char>
         | FullHouse     of List<char>
         | TwoPair       of List<char>
         | OnePair       of List<char>
         | HighCard      of List<char>

      let getHandType (hand : Hand) =
         let grouped =
            groupAndCount hand
            |> List.sortByDescending (fun (c, count) -> count)

         match grouped with
         | [c, 5] -> FiveOfAKind [c]
         | [c, 4; c2, 1] -> FourOfAKind [c; c2]
         | [c, 3; c2, 2] -> FullHouse [c; c2]
         | [c, 3; c2, 1; c3, 1] -> ThreeOfAKind [c; c2; c3]
         | [c, 2; c2, 2; c3, 1] -> TwoPair [c; c2; c3]
         | [c, 2; c2, 1; c3, 1; c4, 1] -> OnePair [c; c2; c3; c4]
         | [c, 1; c2, 1; c3, 1; c4, 1; c5, 1] -> HighCard [c; c2; c3; c4; c5]
         | _ -> failwith "Unknown hand type"

      let scoreHandType (typedHand : TypedHand) =
         match typedHand with
         | FiveOfAKind  _ -> 10
         | FourOfAKind  _ -> 9
         | FullHouse    _ -> 8
         | ThreeOfAKind _ -> 7
         | TwoPair      _ -> 6
         | OnePair      _ -> 5
         | HighCard     _ -> 4


      // compare card lists by finding first card that is greater in one of the two lists
      let compareCardLists (cL1 : List<char>) (cL2 : List<char>) =
         let rec go (cL1 : List<char>) (cL2 : List<char>) =
            match cL1, cL2 with
            | [], [] -> 0
            | c1::c1s, c2::c2s ->
               let compareResult = compareCards c1 c2
               if compareResult = 0 then go c1s c2s
               else compareResult
            | _ -> failwith "Card lists must be the same length"

         go cL1 cL2

      let scoreSameHandType (hand1 : Hand) (hand2 : Hand) =
         let typedHand1 = getHandType hand1
         let typedHand2 = getHandType hand2
         let cl1 = hand1.toCharL
         let cl2 = hand2.toCharL

         match typedHand1, typedHand2 with
         | FiveOfAKind  _, FiveOfAKind  _ -> compareCardLists cl1 cl2 * -1
         | FourOfAKind  _, FourOfAKind  _ -> compareCardLists cl1 cl2 * -1
         | FullHouse    _, FullHouse    _ -> compareCardLists cl1 cl2 * -1
         | ThreeOfAKind _, ThreeOfAKind _ -> compareCardLists cl1 cl2 * -1
         | TwoPair      _, TwoPair      _ -> compareCardLists cl1 cl2 * -1
         | OnePair      _, OnePair      _ -> compareCardLists cl1 cl2 * -1
         | HighCard     _, HighCard     _ -> compareCardLists cl1 cl2 * -1
         | _ -> failwith "Must compare the same kinds of hands"


      let compareHands (hand1 : Hand) (hand2 : Hand) =
         let typedHand1 = getHandType hand1
         let typedHand2 = getHandType hand2

         let score1 = scoreHandType typedHand1
         let score2 = scoreHandType typedHand2
         if score1 = score2 then scoreSameHandType hand1 hand2
         else score1.CompareTo score2


      let naiveSort (handL : List<Hand>) =
         handL |> List.sortWith compareHands

      let go (year, day) runMode =
         let bidM =
            getInput (year, day) runMode
            |> parse
            |> pso "Input: "
            |> List.toMap

         let hands = bidM |> Map.toList |> List.map fst

         let rankM =
            hands
            |> List.sortWith compareHands
            |> List.mapi (fun i hand -> hand, i+1) // add rank
            |> pso "Ranks: "
            |> List.toMap

         hands
         |> List.map (fun hand ->
            let bid = bidM.[hand]
            let rank = rankM.[hand]
            bid * (int64 rank)
            )
         |> List.sum
         |> ps "Answer: "
         // 251201714L answer too high

         rankM
         |> Map.toList
         |> List.sortBy (fun (hand, rank) -> rank)
         |> List.iter (fun (hand, rank) ->
               let handType = getHandType hand
               printn $"Hand: {hand }, HandType : {handType} Rank: {rank}, Bid: {bidM.[hand]}"
            )








   // Part 2
   module Part2 =

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse

   // Run
   let run (year, day) =
      Full |> Part1.go (year, day) |> printn
      //Full |> Part2.go (year, day) |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

      [<Fact>]
      let t1 () =
         let actual = compareCards 'A' '2'
         let expected = -1
         test <@ actual = expected @>

      [<Fact>]
      let t2 () =
         let actual = compareCards '2' 'A'
         let expected = 1
         test <@ actual = expected @>

      [<Fact>]
      let t3 () =
         let actual = compareCards 'A' 'A'
         let expected = 0
         test <@ actual = expected @>

      [<Fact>]
      let ``Compare hands 1`` () =
         let hand1 = Hand ['A'; 'A'; 'A'; 'A'; 'A']
         let hand2 = Hand ['K'; 'K'; 'K'; 'K'; 'K']
         let actual = compareHands hand1 hand2
         let expected = 1
         test <@ actual = expected @>
