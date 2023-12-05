namespace AdventOfCode.Y2023

open Helpers

module Day05 =

   // Part 1
   module Part1 =

      type Mapping = {
         DestinationRangeStart : int64
         SourceRangeStart : int64
         RangeLength : int64
      }


      type TransformMap = {
         SourceCategory : string
         DestinationCategory : string
         MappingL : List<Mapping>
      }

      type Input = {
         Category : string
         ValueL : List<int64>
         TransformL : List<TransformMap>
      }


      let parse (lines : seq<string>) =
         let lineL = lines |> Seq.toList
         let seeds =
            lineL
            |> List.head
            |> fun s -> s.Substring 6
            |> String.splitMultipleOnCharExcl ' '
            |> List.map int64

         let paragraphL =
            lineL
            |> List.skip 2
            |> List.splitMultipleOnExcl (fun s -> s = "")

         let mapL =
            paragraphL
            |> List.map (fun sL ->
                  let [sourceCategory; _; destinationCategory] =
                     sL
                     |> List.head
                     |> String.splitOnceOnChar ' '
                     |> fst
                     |> String.splitMultipleOnCharExcl '-'

                  let mappingL =
                     sL
                     |> List.tail
                     |> List.map (fun s ->
                        let [a; b ;c] =
                           String.splitMultipleOnCharExcl ' ' s
                           |> List.map int64

                        {
                           DestinationRangeStart = a
                           SourceRangeStart = b
                           RangeLength = c
                        }


                     )

                  {
                     SourceCategory = sourceCategory
                     DestinationCategory = destinationCategory
                     MappingL = mappingL
                  }
               )


         {
            Category = "seed"
            ValueL = seeds
            TransformL = mapL
         }

      let convert (sourceRangeStart, rangeLength, destinationRangeStart) n =
         if sourceRangeStart <= n && n < sourceRangeStart + rangeLength then
            Some (n - sourceRangeStart + destinationRangeStart)
         else
            None


      let convert2 (transformMapL : List<Mapping>) (n : int64) =
         let transformedO =
            transformMapL
            |> List.map (fun m -> convert (m.SourceRangeStart, m.RangeLength, m.DestinationRangeStart) n)
            |> List.choose id

         match transformedO with
         | [n'] -> n'
         | [] -> n
         | _ -> failwith "Bang"

      let rec convert3 (category, (valueL : List<int64>)) (transformL : List<TransformMap>)=
         let transformToApplyL =
            transformL
            |> List.filter (fun t -> t.SourceCategory = category)

         match transformToApplyL with
         | [] -> category, valueL
         | [transformToApply] ->
            let x = valueL |> List.map (convert2 transformToApply.MappingL)
            convert3 (transformToApply.DestinationCategory, x) transformL
         | _ -> failwith "Bang2"





      let go (year, day) runMode =
         let parsedInput =
            getInput (year, day) runMode
            |> parse
            |> pso "Input: "

         convert3 (parsedInput.Category, parsedInput.ValueL) parsedInput.TransformL
         |> snd
         |> List.min

         |> pso "Output: "




   // Part 2
   module Part2 =

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> Part1.parse

   // Run
   let run (year, day) =
      Full |> Part1.go (year, day) |> printn
      //Full |> Part2.go (year, day) |> printn


   // Tests
   module Test =
      open Xunit
      open Swensen.Unquote

      open Part1

      [<Fact>]
      let t1 () =
         let input = [ 0; 1; 48; 49; 50; 51; 96; 97; 98; 99 ] |> List.map int64

         let expected = [0; 1; 48; 49; 52; 53; 98; 99; 50; 51] |> List.map int64

         let mappingL = [
            { SourceRangeStart = 98; DestinationRangeStart = 50; RangeLength = 2 }
            { SourceRangeStart = 50; DestinationRangeStart = 52; RangeLength = 48 }
            ]
         let actual =
            input
            |> List.map (convert2 mappingL)

         test <@ expected = actual @>
