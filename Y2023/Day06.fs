namespace AdventOfCode.Y2023

open Helpers

module Day06 =

   // Part 1
   module Part1 =

      type Race = { Time : int; CurrentRecordDistance : int }

      let exampleInput = [
         { Time = 7; CurrentRecordDistance = 9 }
         { Time = 15; CurrentRecordDistance = 40 }
         { Time = 30; CurrentRecordDistance = 200 }
      ]

      let fullInput = [
         { Time = 61; CurrentRecordDistance = 643 }
         { Time = 70; CurrentRecordDistance = 1184 }
         { Time = 90; CurrentRecordDistance = 1362 }
         { Time = 66; CurrentRecordDistance = 1041 }
      ]

      let calculateTravelDistance (buttonHeldTime_ms : int) (availableTime_ms : int) =
         let speed_mm_per_ms = buttonHeldTime_ms
         let remainingTime_ms = availableTime_ms - buttonHeldTime_ms
         let travelDistance = speed_mm_per_ms * remainingTime_ms
         travelDistance


      let go (year, day) runMode =
         fullInput
         |> List.map (fun race ->
               [0..race.Time]
               |> List.map (fun t -> t, calculateTravelDistance t race.Time)
               |> List.filter (fun (t, distance) -> distance > race.CurrentRecordDistance)
               |> List.map fst

               //|> List.reduce (*)

            )
         |> List.map List.length
         |> List.reduce (*)



   // Part 2
   module Part2 =

      type Race = { Time : int64; CurrentRecordDistance : int64 }

      let exampleInput = [
         { Time = 71530; CurrentRecordDistance = 940200 }
      ]

      let fullInput = [
         { Time = 61709066; CurrentRecordDistance = 643118413621041L }
      ]

      let calculateTravelDistance (buttonHeldTime_ms : int64) (availableTime_ms : int64) =
         let speed_mm_per_ms = buttonHeldTime_ms
         let remainingTime_ms = availableTime_ms - buttonHeldTime_ms
         let travelDistance = speed_mm_per_ms * remainingTime_ms
         travelDistance



      type Direction = Up | Down

      // bifurcate from midpoint down
      let findLowestRecordButtonHoldTime availableTime currentRecordDistance =
         printn $"t1, t2, t, isRecord, prevWasRecord, count"

         let rec inner
            (t1, t2, prevWasRecord, direction, count)
            =
               if count > 20 then failwith "Too many iterations"

               let midT = (t1 + t2) / 2L
               let travelDistance = calculateTravelDistance (int midT) availableTime
               let isRecord = travelDistance > currentRecordDistance

               let deltaT =
                  match direction with
                  | Up -> midT - t1
                  | Down -> t2 - midT

               // headings
               printn $"{(t1, t2, midT, isRecord, prevWasRecord, count)}"

               match direction with
               | Down ->
                  match isRecord, prevWasRecord with
                  | false, true when t2 - midT = 1 -> t2





         inner (0L, availableTime, true, Down, 0)

      let go (year, day) =
         fullInput
         |> List.toSeq
         |> Seq.map (fun race ->
               seq [0L..race.Time]
               |> Seq.map (fun t -> t, calculateTravelDistance t race.Time)
               |> Seq.filter (fun (t, distance) -> distance > race.CurrentRecordDistance)
               |> Seq.map fst

               //|> List.reduce (*)

            )
         |> Seq.map Seq.length
         |> Seq.reduce (*)

//
//      let go (_,_) =
//         findLowestRecordButtonHoldTime 71530L 940200L


   // Run
   let run (year, day) =
      //Example |> Part1.go (year, day) |> printn
      Part2.go (year, day) |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

      //[<Fact>]
      //let t1 () =
      //   let input = [""]
      //   let expected = [""]
      //   let actual = parse input
      //   test <@ expected = actual @>
