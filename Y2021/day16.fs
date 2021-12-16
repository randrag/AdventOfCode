namespace AdventOfCode.Y2021

open Helpers

module Day16 =
  let getInput () =
    System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2021/input16.txt")
    |> Seq.head
    |> pso "Parsed input: "

  let decodeHexCharToBits c =
    match c with
    | '0' -> [0; 0; 0; 0]
    | '1' -> [0; 0; 0; 1]
    | '2' -> [0; 0; 1; 0]
    | '3' -> [0; 0; 1; 1]
    | '4' -> [0; 1; 0; 0]
    | '5' -> [0; 1; 0; 1]
    | '6' -> [0; 1; 1; 0]
    | '7' -> [0; 1; 1; 1]
    | '8' -> [1; 0; 0; 0]
    | '9' -> [1; 0; 0; 1]
    | 'A' -> [1; 0; 1; 0]
    | 'B' -> [1; 0; 1; 1]
    | 'C' -> [1; 1; 0; 0]
    | 'D' -> [1; 1; 0; 1]
    | 'E' -> [1; 1; 1; 0]
    | 'F' -> [1; 1; 1; 1]
    | _ -> Unreachable ()

  let bitsToInt64 bits =
    bits
    |> List.map int64
    |> List.foldBack'
         (fun (acc, mult) bit -> (acc + mult * bit, mult * 2L))
         (0L, 1L)
    |> fst

  let bitsToInt bits =
    bits
    |> List.foldBack'
         (fun (acc, mult) bit -> (acc + mult * bit, mult * 2))
         (0, 1)
    |> fst

  let take n bits = List.splitAt n bits

  let takeAndConvertToInt64 n bits =
    List.splitAt n bits |> fun (version, remaining) -> bitsToInt64 version, remaining


  let takeAndConvertToInt n bits =
    List.splitAt n bits |> fun (version, remaining) -> bitsToInt version, remaining

  // reads a literal in groups of five bits, keeping track of the number of bits read
  let rec readLiteral prevLiteralBits readBitCount bits =
    let stopIndicatorBit, bits = takeAndConvertToInt64 1 bits
    let group, bits = take 4 bits
    let literalBits = List.concat2 prevLiteralBits group

    match stopIndicatorBit |> int with
    | 0 -> ((literalBits |> bitsToInt64), (readBitCount + 5), bits) // this is the last group
    | 1 -> readLiteral literalBits (readBitCount + 5) bits // this is not the last group
    | _ -> failwith $"Invalid stop indicator bit {stopIndicatorBit}"

  type LengthTypeId = | BitCount of int | PacketCount of int
  type LiteralData = { Version : int; TypeId : int; Value : int64; CountOfBitsRead : int  }
  and OperatorData = { Version : int; TypeId : int ; LengthTypeId : LengthTypeId; OperatorNumber : int ; SubPackets : List<Packet>  }
  and Packet =
    | Literal of LiteralData
    | Operator of OperatorData

  let rec readPackets alreadyReadPackets remainingBits remainingPacketCount =
    if remainingPacketCount = 0 then alreadyReadPackets, remainingBits
    else
      match remainingBits with
      | remainingBits when List.length remainingBits <= 7 -> alreadyReadPackets, remainingBits // fudge it for now
      | remainingBits  ->

          let version, remainingBits = takeAndConvertToInt 3 remainingBits
          let typeId , remainingBits = takeAndConvertToInt 3 remainingBits

          match typeId |> int with
          | 4 -> // read literal
            let literal, readBitCount, remainingBits = readLiteral [] 6 remainingBits

            let packet = Literal {
                Version = version |> int
                TypeId = typeId |> int
                Value = literal
                CountOfBitsRead = readBitCount
              }

            readPackets (packet::alreadyReadPackets) remainingBits (remainingPacketCount - 1)

          | n -> // an operator packet

            let lengthTypeId, remainingBits = takeAndConvertToInt 1 remainingBits

            match lengthTypeId with
            | 0 -> // next 15 bits are a number that represent the total length in bits of the sub packets contained by this packet
                let length, remainingBits = takeAndConvertToInt 15 remainingBits
                let subPacketBits, remainingBits = take length remainingBits

                let subPackets, remainingSubPacketBits = readPackets [] subPacketBits 9999

                let packet = Operator {
                  Version = version
                  TypeId = typeId
                  LengthTypeId = BitCount length
                  OperatorNumber = n
                  SubPackets = subPackets |> List.rev
                }

                readPackets (packet::alreadyReadPackets) remainingBits (remainingPacketCount - 1)

            | 1 -> // the next 11 bits are a number that represents the number of sub packets immediately contained by this packet
                let subPacketCount, remainingBits = takeAndConvertToInt 11 remainingBits

                let subPackets, remainingBits = readPackets [] remainingBits subPacketCount

                let packet = Operator {
                  Version = version
                  TypeId = typeId
                  LengthTypeId = PacketCount subPacketCount
                  OperatorNumber = n
                  SubPackets = subPackets |> List.rev
                }

                readPackets (packet::alreadyReadPackets) remainingBits (remainingPacketCount - 1)

            | _ -> failwith "never"

  module Part1 =
    let go () =
      let bitStream =
        getInput ()
        |> Seq.collect decodeHexCharToBits
        |> Seq.toList

      let packets, remainingBits = readPackets [] bitStream 99999

      let rec addVersions sum packets =
        match packets with
        | [] -> sum
        | x::xs ->
          match x with
          | Literal p -> addVersions (sum + p.Version) xs
          | Operator p -> addVersions (sum + p.Version) xs + addVersions 0 p.SubPackets

      addVersions 0 packets

  module Part2 =
    let go () =
      let bitStream =
        getInput ()
        |> Seq.collect decodeHexCharToBits
        |> Seq.toList

      let packets, remainingBits = readPackets [] bitStream 99999

      packets |> ps "\n\nPackets: "


      let rec getValue packet =
        match packet with
        | Literal p -> p.Value
        | Operator p ->
           match p.OperatorNumber with
           | 0 -> p.SubPackets |> List.map getValue |> List.reduce (+)
           | 1 -> p.SubPackets |> List.map getValue |> List.reduce (*)
           | 2 -> p.SubPackets |> List.map getValue |> List.reduce min
           | 3 -> p.SubPackets |> List.map getValue |> List.reduce max
           | 5 -> p.SubPackets |> List.map getValue |> fun pl -> if pl.[0] > pl.[1] then 1L else 0L
           | 6 -> p.SubPackets |> List.map getValue |> fun pl -> if pl.[0] < pl.[1] then 1L else 0L
           | 7 -> p.SubPackets |> List.map getValue |> fun pl -> if pl.[0] = pl.[1] then 1L else 0L
           | _ -> failwith "never"

      getValue (List.head packets)

  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "
