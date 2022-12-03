namespace AdventOfCode

open FsToolkit.ErrorHandling
open Helpers


module Day8v2 =
  type Location = int
  type Instruction = | Acc of int | Jmp of int | Nop of int
  type Program = Map<Location, Instruction> // map for fast random access
  type Status = Running | WouldLoop | Terminated
  type State = { pc : int; acc : int }
  type FullState = { State : State; ExecutedLocations : Set<int>; Status : Status }  // program counter * accumulator

  let  input () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day8Input.txt") |> List.ofSeq

  let parseOpCode = function | "acc" -> Acc | "jmp" -> Jmp | "nop" -> Nop | _ -> failwith "Invalid opcode"

  let parseLine (s : string) : Instruction =
    match s.Split(' ') |> List.ofArray with
    | ["acc"; operand] -> Acc (int operand)
    | ["jmp"; operand] -> Jmp (int operand)
    | ["nop"; operand] -> Nop (int operand)
    | _ -> failwith "Invalid input"

  let parseProgram (sL : List<string>) : Program =
    sL
    |> List.mapi (fun (l : Location) s -> (l, parseLine s))
    |> Map.ofList // for fast random access

  let executeInstruction (instruction : Instruction) state =
    match instruction with
    | Acc i -> { pc = state.pc + 1; acc = state.acc + i }
    | Nop _ -> { pc = state.pc + 1; acc = state.acc     }
    | Jmp i -> { pc = state.pc + i; acc = state.acc     }

  let step (program : Program) (fullState : FullState) : FullState =
    match fullState.Status with
    | Running ->
        let newState = executeInstruction (Map.find fullState.State.pc program) fullState.State
        { State = newState
          Status =
            if newState.pc >= Map.count program then Terminated
            elif Set.contains newState.pc fullState.ExecutedLocations then WouldLoop
            else Running
          ExecutedLocations = Set.add newState.pc fullState.ExecutedLocations }
    | WouldLoop  -> fullState
    | Terminated -> fullState

  let getSwappedOpcodeOption = function
    | Jmp i -> Some (Nop i)
    | Nop i -> Some (Jmp i)
    | _ -> None

  let swapProgramInstruction (program : Program) (location : Location)  = option {
    let! newInstruction = getSwappedOpcodeOption (Map.find location program)
    return program |> Map.remove location |> Map.add location newInstruction }

  let getPossiblePrograms (program : Program) =
    [ 0.. Map.count program - 1 ]
    |> List.map (swapProgramInstruction program)
    |> List.filter Option.isSome
    |> List.map Option.get

  let rec runUntil (mustStop : FullState -> bool) program state =
    if mustStop state then state else runUntil mustStop program (step program state )

  let run () =
    let program = input () |> parseProgram

    // part 1
    let initialState = { State = { pc = 0; acc = 0 }; Status = Running; ExecutedLocations = Set.empty }
    do runUntil (fun state -> state.Status = WouldLoop) program initialState
       |> ps "Last state before repeat (part 1): "

    // part 2
    let possiblePrograms = getPossiblePrograms program
    do possiblePrograms
       |> List.map (fun program -> runUntil (fun state -> state.Status <> Running) program initialState)
       |> List.filter (fun state -> state.Status = Terminated)
       |> ps "Last state before terminating (part 2): "
