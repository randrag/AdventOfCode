namespace AdventOfCode
open FsToolkit.ErrorHandling

module Day15 =
  let input = [12; 20; 0; 6; 1; 17; 7]
  module Part1 =

    let findLastSpokenO state number =
      //ps "enter findLastSpokenO with (number, state): " (number, state)
      state
      |> List.tryFind (fun (prevNumber, _) -> prevNumber = number)
      |> Option.map snd // get time

    let getNextEntry state =
      //ps "enter getNextCount with state: "  state
      let currentSpokenNumber = List.head state |> fst
      let lastSpokenAtTimeO = findLastSpokenO (List.tail state) currentSpokenNumber
      let currentTime = state |> List.head |> snd
      match lastSpokenAtTimeO with
      | None -> 0, currentTime + 1
      | Some time -> currentTime - time, currentTime + 1

    let rec makeList2 n startingList  : List<int * int> =
        //ps "\nstarting list: " startingList
        if startingList |> List.head |> snd = n then startingList
        else
            let nextEntry = getNextEntry startingList //|> pso "Next count: "
            makeList2  n (nextEntry :: startingList)

    let addTimeToInput = List.mapi (fun time' numberSpoken -> numberSpoken, time' + 1) >> List.rev

    let run () =
      //let input = [0; 3; 6]

      input
      |> addTimeToInput
      |> makeList2 2020
      |> pso "Last: "
      |> ignore

  module Part2 =
    //let input = [3; 1; 2]

    let getNextSpoken justSpoken currentTime (previouslySpokenAtMap : Map<int,List<int>>) =
      let previouslySpokenAtO = Map.tryFind justSpoken previouslySpokenAtMap
      match previouslySpokenAtO with
      | None -> 0
      | Some times ->
        let previousTime = List.filter ((<>) currentTime) times |> List.max
        currentTime - previousTime

    let takeUpTo n l = if List.length l <= n then l else List.take n l

    let rec getNumberSpokenAtTime timeToStop currentTime justSpoken (previouslySpokenAtMap : Map<int,List<int>>) =
      //ps "enter with (timeToStop, currentTime, justSpoken, history): " (timeToStop, currentTime, justSpoken, previouslySpokenAtMap)
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

        let nextMap = Map.add numberToSpeak ((currentTime :: numberToSpeakSpokenAtTimes) |> takeUpTo 2) previouslySpokenAtMap

        if currentTime % 1000000 = 0 then ps "current time: " currentTime

        getNumberSpokenAtTime timeToStop (currentTime + 1) numberToSpeak nextMap


    let run () =
      let currentTime = List.length input + 1
      let justSpoken = List.last input
      input
      |> Part1.addTimeToInput
      |> List.map (fun (spoken, time) -> spoken, [time])
      |> Map.ofList
      |> getNumberSpokenAtTime 30000000 currentTime justSpoken
      |> pso "Last: "
      |> ignore



  let run () = Part2.run ()
