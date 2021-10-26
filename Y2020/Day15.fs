namespace AdventOfCode
open FsToolkit.ErrorHandling

module Day15 =
  let input = [12; 20; 0; 6; 1; 17; 7]
  module Part1 =

    let findLastSpokenO state number =
      state
      |> List.tryFind (fun (prevNumber, _) -> prevNumber = number)
      |> Option.map snd // get time

    let getNextEntry state =
      let currentSpokenNumber = List.head state |> fst
      let lastSpokenAtTimeO = findLastSpokenO (List.tail state) currentSpokenNumber
      let currentTime = state |> List.head |> snd
      match lastSpokenAtTimeO with
      | None -> 0, currentTime + 1
      | Some time -> currentTime - time, currentTime + 1

    let rec makeList2 n startingList  : List<int * int> =
        if startingList |> List.head |> snd = n then startingList
        else
            let nextEntry = getNextEntry startingList
            makeList2  n (nextEntry :: startingList)

    let addTimeToInput = List.mapi (fun time' numberSpoken -> numberSpoken, time' + 1) >> List.rev

    let run () =

      input
      |> addTimeToInput
      |> makeList2 2020
      |> pso "Last: "
      |> ignore

  module Part2 =

    let rec getNumberSpokenAtTime timeToStop currentTime justSpoken (previouslySpokenAtMap : Map<int,List<int>>) =

      if timeToStop = currentTime - 1 then justSpoken
      else
        let previouslySpokenAtTimes = Map.tryFind justSpoken previouslySpokenAtMap |> Option.defaultValue []

        let mostRecentlySpokenAt =
          previouslySpokenAtTimes
          |> List.filter ((<>) (currentTime - 1))
          |> List.tryHead

        let numberToSpeak =
          mostRecentlySpokenAt
          |> function
             | None -> 0
             | Some time -> currentTime - time - 1

        let numberToSpeakSpokenAtTimes = Map.tryFind numberToSpeak previouslySpokenAtMap |> Option.defaultValue []

        let nextMap = Map.add numberToSpeak ((currentTime :: numberToSpeakSpokenAtTimes) |> List.truncate 2) previouslySpokenAtMap
        if currentTime % 1000000 = 0 then printf "."

        getNumberSpokenAtTime timeToStop (currentTime + 1) numberToSpeak nextMap

    let run () =

      let currentTime = List.length input + 1
      let justSpoken = List.last input

      input
      |> Part1.addTimeToInput
      |> List.map (fun (spoken, time) -> spoken, [time])
      |> Map.ofList
      |> getNumberSpokenAtTime 2020 currentTime justSpoken
      |> pso "Last: "
      |> ignore





  let run () = Part2.run ()
