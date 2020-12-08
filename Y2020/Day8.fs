namespace AdventOfCode
open FsToolkit.ErrorHandling

type Position = int
type OpCode = string
type Operand = int
type State = int * int // program counter * accumulator

type Instruction = Position * OpCode * Operand
type Program = array<Instruction> // array for fast random access
module Day8 =

// parsing of input
  let input = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day8Input.txt") |> List.ofSeq

  let parseLine (s : string) =
    let wordL = s.Split(' ') |> List.ofArray
    match wordL with
    | [opcode; operand] -> opcode, operand |> int
    | _ -> failwith "Invalid input"

  let parseProgram (sL : List<string>) =
      sL
      |> List.mapi (fun i s ->
           let (opcode, operand) = parseLine s
           (i, opcode, operand))
      |> Array.ofList // for fast random access
      |> pso "Program: "

  let executeInstruction acc instruction =
    match instruction with
    | (programCounter, "acc", i) -> programCounter + 1, acc + i
    | (programCounter, "nop", _) -> programCounter + 1, acc
    | (programCounter, "jmp", o) -> programCounter + o, acc
    | _ -> failwith "unknown instruction"

  let executeNextInstruction (program : Program) ( pc , acc ) =
    if pc >= Array.length program then
      failwithf "Program being interpreted terminated with state: %A" (pc, acc)
    else
      executeInstruction acc program.[pc]

  let rec runProgram (program : Program) (maxSteps : int) ((programCounter, acc) : State) =
    match maxSteps with
    | 0 -> programCounter, acc
    | maxSteps ->
        let newState = executeNextInstruction program (programCounter, acc)
        runProgram program (maxSteps-1) newState

  // returns Some instruction if changed, otherwise none
  let swapOpcode ( instruction : Instruction ) =
    match instruction with
    | pos, "jmp", operand ->  Some (pos, "nop", operand)
    | pos, "nop", operand ->  Some (pos, "jmp", operand)
    | _ -> None

  let swapProgramInstruction (position : Position) (program : Program) = option {
    let! newInstruction = swapOpcode program.[position]
    let a = program |> Array.take position
    let b = program |> Array.skip (position + 1)
    return (Array.concat (seq { a; [| newInstruction |]; b } ) : Program)
    }

  let run () =
    let program = input |> parseProgram

    // Part 1, in imperative style with while loop
    let mutable visitedLinesArray = Array.zeroCreate (Array.length program)
    let mutable pc = 0
    let mutable acc = 0

    let mutable lastPc = 0
    let mutable lastAcc = 0

    while visitedLinesArray.[pc] = 0 do
      lastPc <- pc
      lastAcc <- acc
      visitedLinesArray.[pc] <- 1
      let (pc', acc') = executeInstruction acc program.[pc]
      pc <- pc'
      acc <- acc'

    ps "Part 1 result: pc, acc: " (lastPc, lastAcc)
    nl

    let possiblePrograms =
      [ 0 .. ((Array.length program) - 1) ]
      |> List.map (fun i -> swapProgramInstruction i program)
      |> List.filter Option.isSome
      |> List.map Option.get

    let programCount = List.length possiblePrograms
    let startingStateL =
      [1 ..programCount]
      |> List.map (fun _ -> (0, 0) : State)

    let stepAll programL stateL =
      List.map2 (fun program state -> executeNextInstruction program state ) programL stateL

    let rec run' programs states =
      let ns = stepAll programs states
      run' programs ns

    ps "Next states (unreached - result is currently in the exception): " (run' possiblePrograms startingStateL)

    ()







