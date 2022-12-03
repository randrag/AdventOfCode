namespace AdventOfCode
open Helpers
module Day12 =

  let  input  = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input12b.txt")


  type ShipState = {
      Heading : int
      xPosition : int
      yPosition : int
    }

  type WayPointState = {
    xPosition : int
    yPosition : int
  }

  type Command =
  | MoveN of int
  | MoveS of int
  | MoveE of int
  | MoveW of int
  | MoveForward of int
  | TurnCW of int
  | TurnCCW of int

  module Common =

    let parseCommand s =
      let l = s.ToString().Chars(0)
      let n = s.ToString().Substring(1) |> int

      match (l, n) with
        | 'N', n -> MoveN n
        | 'S', n -> MoveS n
        | 'E', n -> MoveE n
        | 'W', n -> MoveW n
        | 'F', n -> MoveForward n
        | 'R', n -> TurnCW n
        | 'L', n -> TurnCCW n
        | _ -> failwith "Unknown command encountered"

    let commands = input |> Seq.map parseCommand

  module Part1 =
    open Common

    let run () =

      let rec interpret (state : ShipState) command : ShipState =
        match command with
        | MoveN n -> { state with yPosition = state.yPosition + n }
        | MoveS n -> { state with yPosition = state.yPosition - n }
        | MoveE n -> { state with xPosition = state.xPosition + n }
        | MoveW n -> { state with xPosition = state.xPosition - n }
        | MoveForward n ->
          match state.Heading with
          | 0   -> interpret state (MoveN n)
          | 180 -> interpret state (MoveS n)
          | 270 -> interpret state (MoveW n)
          | 90  -> interpret state (MoveE n)
          | _ -> failwith "weird heading"
        | TurnCW  n -> { state with Heading = (state.Heading + n) % 360 }
        | TurnCCW n -> { state with Heading = (state.Heading - n + 360) % 360 }

      let initialState = { Heading = 90; xPosition = 0; yPosition = 0 }
      let finalState = Seq.fold interpret initialState commands |> pso "Final state part 1: "
      do finalState |> fun r -> abs r.xPosition + abs r.yPosition |> ps "Final manhattan position: "
      ()

  module Part2 =
    open Common
    let run () =

      let rec interpret ((shipState : ShipState), (waypointState : WayPointState)) command =
        match command with
        | MoveN n -> shipState, { waypointState with yPosition = waypointState.yPosition + n }
        | MoveS n -> shipState, { waypointState with yPosition = waypointState.yPosition - n }
        | MoveE n -> shipState, { waypointState with xPosition = waypointState.xPosition + n }
        | MoveW n -> shipState, { waypointState with xPosition = waypointState.xPosition - n }
        | TurnCW n ->
          match n with
          | 0 ->   shipState, waypointState
          | 90 ->  shipState, { waypointState with xPosition =  waypointState.yPosition; yPosition = -waypointState.xPosition }
          | 180 -> shipState, { waypointState with xPosition = -waypointState.xPosition; yPosition = -waypointState.yPosition }
          | 270 -> shipState, { waypointState with xPosition = -waypointState.yPosition; yPosition =  waypointState.xPosition }
          | _ -> failwith "weird direction"
        | TurnCCW n ->
          interpret (shipState, waypointState) (TurnCW ((360 - n ) % 360))
        | MoveForward n ->
          let newShipState = { shipState with
                                 xPosition = shipState.xPosition + n * waypointState.xPosition
                                 yPosition = shipState.yPosition + n * waypointState.yPosition }
          newShipState, waypointState

      let initialState = { Heading = 90; xPosition = 0; yPosition = 0 }, { xPosition = 10; yPosition = 1 }
      let finalState = Seq.fold interpret initialState commands |> pso "Part 2 final state: "
      do finalState |> fun (r, _) -> abs r.xPosition + abs r.yPosition |> ps "Final manhattan position: "
      ()
