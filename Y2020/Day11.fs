namespace AdventOfCode

module Day11 =

  module Common =
    let  input () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input11.txt")

    type Position = int * int

    /// returns a list containing up to 8 positions directly around the seat
    let getPositionsAroundSeat ((x,y) : Position) : seq<Position> =
         seq {(x - 1, y - 1); (x    , y - 1); (x + 1, y - 1)
              (x - 1, y - 0);  (x + 1, y - 0)
              (x - 1, y + 1);  (x    , y + 1);  (x + 1, y + 1)}



    /// given a map and a position, gives you the seat at that position, if the position falls on the map
    let getSeatAtPositionO (map : Map<Position, char>) pos : Option<char> =
      map |> Map.tryFind pos

    let getSeatAtPositionX (map : Map<Position, char>) pos : char =
      map |> Map.find pos


    /// returns a list of all positions on the map
    let getAllPositions (map : Map<Position, char>) =
      map |> Map.toSeq |> Seq.map (fun (pos, _) -> pos)

    /// given a threshold, current status and count of occupied seats, returns the new status
    let decideNewStatus threshold currentStatus countOfOccupied =
      match currentStatus, countOfOccupied with
      | 'L', 0 -> '#'
      | '#', n when n >= threshold -> 'L'
      | _ -> currentStatus

    /// calculate new map by applying threshold rule to the current map
    let calcNewMap (threshold : int) (map : Map<Position, char * int> ) =
      map |> Map.map (fun _ (status, count) -> decideNewStatus threshold status count )

    /// Help me see what is happening
    let sprintMap (map : Map<Position, char>) =
      map
      |> Map.toSeq
      |> Seq.groupBy (fun ((_, y), _) -> y)
      |> Seq.map (fun (_, rows) -> rows |> Seq.map (fun (_,c) -> c ) |> SeqOfChar.toString )
      |> Seq.map (sprintf "%s \n")
      |> Seq.reduce ( + )
      |> fun s -> "\n" + s


  module Part1 =
    open Common
    /// given a map and a position, counts the number of occupied seats around the position
    let countOccupiedAroundSeat
      (map : Map<Position,char>)
      ((x, y) : Position) =

         getPositionsAroundSeat (x,y)
         |> Seq.map (getSeatAtPositionO map)
         |> Seq.map (fun seatStatusO ->
            match seatStatusO with
            | Some '#' -> 1
            | _ -> 0 )
         |> Seq.sum

    /// calculates the occupied count for each seat according to part 1's algorithm
    let addOccupiedCountToMap (map : Map<Position, char>) =
      map
      |> Map.map (fun pos _ ->
          let countOfOccupied = countOccupiedAroundSeat map pos
          let status = getSeatAtPositionO map pos |> Option.get
          (status, countOfOccupied)
          )

    /// Iterate until map no longer changes
    let rec findStaticMap n threshold (map : Map<Position, char>) =
      do ps "Iteration: n: " n
      do ps "Map: " (sprintMap map)
      let nextMap = addOccupiedCountToMap map |> calcNewMap threshold
      if nextMap <> map then findStaticMap (n + 1) threshold nextMap else nextMap

  module Part2 =
    open Common
    /// Returns a list of lists for each of the 8 rays emanating from the seat
    let getLinesOfSightAroundSeat (maxX, maxY) ((x,y) : Position) : seq<seq<Position>> =

      let rec combine acc (xs : seq<int>) (ys : seq<int>) =
        if (Seq.isEmpty xs) || (Seq.isEmpty ys) then acc |> Seq.rev
        else
          let x', xs' = Seq.head xs, Seq.tail xs
          let y', ys' = Seq.head ys, Seq.tail ys

          combine (Seq.concat (seq { seq {(x', y')}; acc })) xs' ys'

      let l = { x - 1 ..  -1 .. 0    }
      let r = { x + 1 ..   1 .. maxX }
      let u = { y - 1 ..  -1 .. 0    }
      let d = { y + 1 ..   1 .. maxY }
      let ul = combine [] l u
      let ur = combine [] r u
      let dl = combine [] l d
      let dr = combine [] r d
      let u' = u |> Seq.map (fun y' -> (x, y'))
      let d' = d |> Seq.map (fun y' -> (x, y'))
      let l' = l |> Seq.map (fun x' -> (x', y))
      let r' = r |> Seq.map (fun x' -> (x', y))

      seq {u'; ur; r'; dr; d'; dl; l'; ul}

    /// given a map and a position, counts the number of VISIBLE occupied seats around the position
    let countVisibleOccupiedAroundSeat
      (map : Map<Position,char>)
      ((x, y) : Position) =
        let maxX = getAllPositions map |> Seq.map (fun (x,_) -> x) |> Seq.max
        let maxY = getAllPositions map |> Seq.map (fun (_,y) -> y) |> Seq.max
        getLinesOfSightAroundSeat (maxX, maxY) (x,y)
        |> Seq.map (fun visiblePositions ->
            visiblePositions
            |> Seq.map (getSeatAtPositionO map >> Option.get)
            |> Seq.filter ( (<>) '.')
            |> Seq.tryHead |> Option.map ( (=) '#' ) |> Option.defaultValue false

        ) |> Seq.sumBy (fun containsOccupied -> if containsOccupied then 1 else 0)

    /// calculates the occupied count for each seat according to part 2's algorithm
    let addOccupiedCountToMap (map : Map<Position, char>) =
      map
      |> Map.map (fun position _ ->
          let countOfOccupied = countVisibleOccupiedAroundSeat map position
          let status = getSeatAtPositionO map position |> Option.get
          (status, countOfOccupied)
          )

    /// Iterate until map no longer changes
    let rec findStaticMap n threshold (map : Map<Position, char>) =
      // do ps "Map: " (sprintMap map)
      do ps "Iteration: n: " n
      do ps "Map: " (sprintMap map)

      printf "."
      let nextMap = addOccupiedCountToMap map |> calcNewMap threshold
      printf "-"
      if nextMap <> map then findStaticMap (n + 1) threshold nextMap else nextMap

  module Part2Again =

    open Common

    let wasThresholdExceeded map threshold (linesOfSight : seq<seq<Position>>) =
      let rec inner (acc : int) (linesOfSight : seq<seq<Position>>) =

        if Seq.isEmpty linesOfSight then false
        else
          linesOfSight
          |> Seq.head
          |> Seq.map (getSeatAtPositionX map)
          |> Seq.filter ((<>) '.') // find first seat
          |> Seq.tryHead // there might not be one
          |> function
             | Some '#' -> if acc + 1 > threshold then true
                                else inner (acc + 1) (Seq.tail linesOfSight)
             | _ -> inner acc (Seq.tail linesOfSight)

      inner 0 linesOfSight

    let calcNewMap2 (map : Map<Position, char>) =
      map
      |> Map.map (fun position seatStatus ->
          match seatStatus with
          | '.' -> '.'
          | '#' -> // occupied flips to Empty if more than 5 seats are occupied
              let maxX = getAllPositions map |> Seq.map (fun (x,_) -> x) |> Seq.max
              let maxY = getAllPositions map |> Seq.map (fun (_,y) -> y) |> Seq.max
              let linesOfSight = Part2.getLinesOfSightAroundSeat (maxX, maxY) position
              if wasThresholdExceeded map 4 linesOfSight then 'L' else '#'
          | 'L' -> // empty seats get occupied if there are no seats occupied
              let maxX = getAllPositions map |> Seq.map (fun (x,_) -> x) |> Seq.max
              let maxY = getAllPositions map |> Seq.map (fun (_,y) -> y) |> Seq.max
              let linesOfSight = Part2.getLinesOfSightAroundSeat (maxX, maxY) position
              if wasThresholdExceeded map 0 linesOfSight then 'L' else '#'
          | _ -> failwith "Never"



        )

    let countSeatsInMap m =
      m
      |> Map.toSeq
      |> Seq.map (fun (_, seatStatus) -> match seatStatus with | '#' -> 1 | _ -> 0)
      |> Seq.sum

    /// Iterate until map no longer changes
    let rec findStaticMap n threshold (map : Map<Position, char>) =
      // do ps "Map: " (sprintMap map)
      do ps "Iteration: n: " n
      do ps "Map: " (sprintMap map)

      printf "."
      let nextMap = calcNewMap2 map
      printf "-"
      if nextMap <> map then findStaticMap (n + 1) threshold nextMap else nextMap

  module Run =
    open Common
    let run () =
      let floorMap =
        input ()
        |> Seq.mapi (fun y string -> // rows
             string |> Seq.mapi (fun x char -> // columns
               (x, y), char))
        |> Seq.concat
        |> Map.ofSeq

      // part 1
      do
        Part1.findStaticMap 0 4 floorMap
        |> Map.toSeq
        |> Seq.map (fun (_, seatStatus) -> match seatStatus with | '#' -> 1 | _ -> 0)
        |> Seq.sum
        |> ignore //|> ps "Part 1 answer: "


      // part 2
      printfn "PART 2 Starts here"
      printfn "%A" (Part2.getLinesOfSightAroundSeat (9,9) (8,0) |> Seq.map (List.ofSeq) |> List.ofSeq)
      do
        Part2Again.findStaticMap 0 5 floorMap
        |> Part2Again.countSeatsInMap
        |> ps "Part 2 answer: "



      ()
