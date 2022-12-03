namespace AdventOfCode

open Helpers

module Day14 =

  let  input_a ()  = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input14a.txt")
  let  input_b () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input14b.txt")
  let  input_c () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Input14c.txt")

  module Part1 =

    type Mask = {
      And : uint64
      Or : uint64
    }

    type ProgramLine =
      | SetMask of Mask
      | SetMemory of int * uint64

    type State = {
      Mask : Mask // orMask, andMask
      Memory : Map<int, uint64> // address, value
    }

    let parseBits (chars : List<char>) : Mask =
      let rec inner mask chars =
        match chars with
        | [] -> mask
        | '0' :: cs -> inner { Or = ( mask.Or <<< 1);       And = (mask.And <<< 1)      } cs
        | '1' :: cs -> inner { Or = ( mask.Or <<< 1) + 1UL; And = (mask.And <<< 1) + 1UL} cs
        | 'X' :: cs -> inner { Or = ( mask.Or <<< 1);       And = (mask.And <<< 1) + 1UL} cs
        | _ -> failwith "Invalid mask char"

      inner { And = 0UL; Or = 0UL} chars

    let parseLine (s : string) : ProgramLine =
      if s.StartsWith("mask = ") then s.Remove(0,7) |> List.ofSeq |> parseBits |> SetMask
      else
        let pos = s.Substring(4, s.IndexOf(']') - s.IndexOf('[') - 1) |> int
        let value = s.Substring (s.IndexOf " = " + 3) |> uint64
        SetMemory (pos, value)

    let parse (input : seq<string>) = input |> Seq.map parseLine

    let interpret (state : State) programLine : State =
      match programLine with
      | SetMask mask -> { state with  Mask = mask }
      | SetMemory (address, value) ->
          let valueToWrite =  (value &&& state.Mask.And) ||| state.Mask.Or
          { state with Memory = Map.add address valueToWrite state.Memory }

    let initialState = { State.Mask = {And = 0UL; Or = 0UL }; State.Memory = Map.empty }

    let run () =
      input_b ()
      |> parse
      |> List.ofSeq
      |> List.fold interpret initialState
      |> fun state -> state.Memory
      |> Map.toList
      |> List.sumBy (fun (_, value) -> value)
      |> ps "Answer part 1: "

  module Part2 =


    let getOffsets bitPositions =

      let rec inner acc remainingBitPositions =
        match remainingBitPositions with
        | [] -> acc
        | x::xs ->
            let bitValue = (1UL <<< x)
            let additionalValues =  acc |> List.map ((+) bitValue)
            inner (additionalValues @ acc) xs

      inner [0UL] bitPositions

    let bitsToInt (bits : List<bool>) =
      let rec inner acc bits =
        match bits with
        | [] -> acc
        | false :: cs -> inner  (acc <<< 1)        cs
        | true  :: cs -> inner ((acc <<< 1) + 1UL) cs
      inner 0UL bits

    /// return list of positions of 'floating' bits mask
    let getBitPositions (cs : List<char>) : List<int> =
      cs
      |> List.rev
      |> List.mapi (fun n c -> (n, c))
      |> List.filter (snd >> ((=) 'X'))
      |> List.map fst

    type ProgramLine = | Mask of uint64 * uint64 * List<uint64> | Mem of address : uint64 * value : uint64

    let parseLine (s : string) : ProgramLine =
      if s.StartsWith("mask = ")
      then
        let maskChars = s.Remove(0,7) |> List.ofSeq
        let ones = maskChars |> List.map ( (=) '1') |> bitsToInt
        let Xs   = maskChars |> List.map ( (=) 'X') |> bitsToInt
        let offsets = maskChars |> getBitPositions |> getOffsets
        Mask (ones, Xs, offsets)
      else
        let pos = s.Substring(4, s.IndexOf(']') - s.IndexOf('[') - 1) |> uint64
        let value = s.Substring (s.IndexOf " = " + 3) |> uint64
        Mem (pos, value)

    let parse strings = Seq.map parseLine strings |> List.ofSeq

      /// take the address
      /// or in the 1s in the mask
      /// and in the Xs in the mask
      /// then or in all the offsets
      /// This gives you all the addresses that need to have the value written

    let getAddresses (ones, Xs, offsets) address =
        let preMaskedAddress = address ||| ones |> (&&&) (~~~Xs)
        offsets |> List.map ((|||) preMaskedAddress)

    type State = {
      Mask :  uint64 * uint64 * List<uint64> // orMask, andMask
      Memory : Map<uint64, uint64> // address, value
    }

    let initialState = { Mask = (0UL, 0UL, []); Memory = Map.empty }

    let interpret state command =
      match command with
      | Mask (ones, Xs, offsets) -> { state with Mask = (ones, Xs, offsets) }
      | Mem (address, value) ->
          let addresses = getAddresses state.Mask address
          let x = addresses |> List.fold (fun map address -> Map.add address value map) state.Memory
          { state with Memory = x }

    type myDU = | PushToEvoNo of int | Case2 of int
    let run () =

      input_b ()
      |> parse
      |> List.fold interpret initialState
      |> fun state -> state.Memory
      |> Map.toList
      |> List.map (fun (key, value) -> value)
      |> List.reduce (+)
      |> pso "Part 2 answer: "
      |> ignore

      let x = PushToEvoNo 1
      let getFirstBitOutOfIt (s: string) = s.Split ' ' |> Seq.head
      let p x = sprintf $"{x}"

      printfn $"Test interpolating {getFirstBitOutOfIt (p x)} with name {nameof(x)} of type {typeof<myDU>} of typedef of {typedefof<myDU>}"


  let run () = Part2.run ()
