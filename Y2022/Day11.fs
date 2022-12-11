namespace AdventOfCode.Y2022

open Helpers

module Day11 =

   module Part1 =

      type Operand = | Old | N of int64
      type Operator = | Add | Multiply

      type Operation =
         {
            Operator : Operator
            Operand1 : Operand
            Operand2 : Operand
            }

      type MonkeyState = {
         WorryLevelL : List<int64>
         Operation : Operation
         TestDivisor : int64
         DestIfTrue : int
         DestIfFalse : int
         InspectionCount : int64
      }

      let parseOperand (s : string) =
         if s = "old" then Old
         else N (s |> String.parseToInt64X)

      let parseOperator (s : string) =
         match s with
         | "*" -> Multiply
         | "+" -> Add


      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.splitMultipleOnExcl (fun s -> s = "")
         |> List.mapi (fun i sL ->
               ( i
               , match sL with
                 | [l1; l2; l3; l4; l5; l6] -> {
                    WorryLevelL =
                        l2.Split "  Starting items: "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> fun s -> s.Split ","
                        |> Array.toList
                        |> List.map String.parseToInt64X
                    Operation =
                        l3.Split "  Operation: new = old "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> fun s -> s.Split (" ")
                        |> Array.toList
                        |> fun s ->
                           match s with
                           | [s1; s2] -> { Operator = s1 |> parseOperator; Operand1 = Old; Operand2 = parseOperand s2 }
                    TestDivisor =
                        l4.Split "  Test: divisible by "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> String.parseToInt64X
                    DestIfTrue =
                        l5.Split "    If true: throw to monkey "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> String.parseToIntX
                    DestIfFalse =
                        l6.Split "    If false: throw to monkey "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> String.parseToIntX
                    InspectionCount = 0
                        }
               ))
         |> List.toMap


      let go (year, day) runMode =
         let monkeys =
            getInput (year, day) runMode
            |> parse
            |> id

         let rec turn activeMonkeyN (monkeys : Map<int,MonkeyState>)  =
            let activeMonkey = monkeys[activeMonkeyN]
            match activeMonkey.WorryLevelL with
            | [] -> monkeys
            | x :: xs ->

               let worryLevel =
                  match activeMonkey.Operation.Operator, activeMonkey.Operation.Operand2 with
                  | Multiply, N n-> x * n
                  | Add, N n -> x + n
                  | Multiply, Old -> x * x
                  | Add, Old -> x + x

               let targetMonkeyN =
                 if worryLevel % activeMonkey.TestDivisor = 0 then activeMonkey.DestIfTrue
                 else activeMonkey.DestIfFalse

               let targetMonkey = Map.find targetMonkeyN monkeys
               let l = List.concat [targetMonkey.WorryLevelL; [worryLevel]]
               let targetMonkey' = { targetMonkey with WorryLevelL = l }
               let activeMonkey' = {
                  activeMonkey with
                     WorryLevelL = xs
                     InspectionCount = activeMonkey.InspectionCount + 1L
               }
               let monkeys' =
                  monkeys
                  |> Map.add activeMonkeyN activeMonkey'
                  |> Map.add targetMonkeyN targetMonkey'
               turn activeMonkeyN monkeys'

         let maxMonkeyN =
            monkeys
            |> Map.toList
            |> List.map fst
            |> List.max

         let rec round (max, current) monkeys  =
            if current <= max then
               let monkeys' = turn current monkeys
               round (max, current+1) monkeys'
            else monkeys

         //round (maxMonkeyN, 0) monkeys

         let rec myFold remainingRounds monkeys =
            if remainingRounds > 0 then
               let monkeys' = round (maxMonkeyN, 0) monkeys
               myFold (remainingRounds - 1) monkeys'
            else monkeys

         myFold 20 monkeys
         |> Map.toList
         |> List.map snd
         |> List.map (fun monkey -> monkey.InspectionCount)
         |> List.sortDescending
         |> List.take 2
         |> List.reduce ( * )


   module Part2 =

      type Operand = | Old | N of int64
      type Operator = | Add | Multiply

      type Operation =
         {
            Operator : Operator
            Operand1 : Operand
            Operand2 : Operand
            }


      type MonkeyState = {
         WorryLevelL : List<int64>
         Operation : Operation
         TestDivisor : int64
         DestIfTrue : int
         DestIfFalse : int
         InspectionCount : int64
      }

      let parseOperand (s : string) =
         if s = "old" then Old
         else N (s |> String.parseToInt64X)

      let parseOperator (s : string) =
         match s with
         | "*" -> Multiply
         | "+" -> Add

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.splitMultipleOnExcl (fun s -> s = "")
         |> List.mapi (fun i sL ->
               ( i
               , match sL with
                 | [l1; l2; l3; l4; l5; l6] -> {
                    WorryLevelL =
                        l2.Split "  Starting items: "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> fun s -> s.Split ","
                        |> Array.toList
                        |> List.map String.parseToInt64X
                    Operation =
                        l3.Split "  Operation: new = old "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> fun s -> s.Split (" ")
                        |> Array.toList
                        |> fun s ->
                           match s with
                           | [s1; s2] -> { Operator = s1 |> parseOperator; Operand1 = Old; Operand2 = parseOperand s2 }
                    TestDivisor =
                        l4.Split "  Test: divisible by "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> String.parseToInt64X
                    DestIfTrue =
                        l5.Split "    If true: throw to monkey "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> String.parseToIntX
                    DestIfFalse =
                        l6.Split "    If false: throw to monkey "
                        |> Array.toList
                        |> List.tail
                        |> List.head
                        |> String.parseToIntX
                    InspectionCount = 0
                        }
               ))
         |> List.toMap


      let go (year, day) runMode =
         let monkeys =
            getInput (year, day) runMode
            |> parse

         let divisorMult =
            monkeys
            |> Map.toListOfValuesSortedBy None
            |> List.map (fun ms -> ms.TestDivisor)
            |> List.reduce (*)

         let rec turn activeMonkeyN (monkeys : Map<int,MonkeyState>) commonDivisor  =
            let activeMonkey = monkeys[activeMonkeyN]
            match activeMonkey.WorryLevelL with
            | [] -> monkeys
            | x :: xs ->

               let worryLevel =
                  match activeMonkey.Operation.Operator, activeMonkey.Operation.Operand2 with
                  | Add, N n -> x + n
                  | Add, Old -> x + x
                  | Multiply, N n-> x * n
                  | Multiply, Old -> x * x

               let worryLevel = worryLevel % commonDivisor

               let targetMonkeyN =
                 if worryLevel % activeMonkey.TestDivisor = 0 then activeMonkey.DestIfTrue
                 else activeMonkey.DestIfFalse

               let targetMonkey = Map.find targetMonkeyN monkeys
               let l = List.concat [targetMonkey.WorryLevelL; [worryLevel]]
               let targetMonkey' = { targetMonkey with WorryLevelL = l }
               let activeMonkey' = {
                  activeMonkey with
                     WorryLevelL = xs
                     InspectionCount = activeMonkey.InspectionCount + 1L
               }
               let monkeys' =
                  monkeys
                  |> Map.add activeMonkeyN activeMonkey'
                  |> Map.add targetMonkeyN targetMonkey'
               turn activeMonkeyN monkeys' commonDivisor

         let maxMonkeyN =
            monkeys
            |> Map.toList
            |> List.map fst
            |> List.max

         let rec round (max, current) commonDivisor monkeys  =
            if current <= max then
               let monkeys' = turn current monkeys commonDivisor
               round (max, current+1) commonDivisor monkeys'
            else monkeys

         //round (maxMonkeyN, 0) monkeys

         let rec myFold remainingRounds monkeys =
            if remainingRounds > 0 then
               let monkeys' = round (maxMonkeyN, 0) divisorMult monkeys
               myFold (remainingRounds - 1) monkeys'
            else monkeys

         myFold 10000 monkeys
         |> Map.toList
         |> List.map snd
         |> List.map (fun monkey -> monkey.InspectionCount)
         |> List.sortDescending
         |> List.take 2
         |> List.reduce ( * )






   let run (year, day) =
      //Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
