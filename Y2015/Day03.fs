namespace AdventOfCode.Y2015

module Day03 =
  let loadLines () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2015/input03.txt") |> Seq.collect id

  let calcPos (x,y) c =
    match c with
    | '<' -> (x - 1, y)
    | '>' -> (x + 1, y)
    | '^' -> (x, y - 1)
    | 'v' -> (x, y + 1)
    | _ -> failwith "Unexpected char"

  let startState = ((0,0), (Set.singleton (0,0)))

  module Part1 =
    let step (currentPos, set) c =
            let nextPos = calcPos currentPos c
            let nextSet = Set.add nextPos set
            (nextPos, nextSet)

    let run () =
      let count = loadLines () |> Seq.fold step startState |> snd |> Set.count
      printfn "%A" count

  module Part2 =
    type Who = | Santa | Robo
    let step (whoNext, santaState, roboState) c =
      match whoNext with
      | Santa -> (Robo, Part1.step santaState c, roboState)
      | Robo -> (Santa, santaState, Part1.step roboState c)

    let (_, finalSantaState, finalRoboState) = loadLines () |> Seq.fold step (Santa, startState, startState)

    let allVisitedPositionCount = Set.union (snd finalSantaState) (snd finalRoboState) |> Set.count

    let run () =  printfn "Count of all visited positions: %A" allVisitedPositionCount
