namespace AdventOfCode.Y2022

open Helpers

module Day08 =

   module Part1 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.map Seq.toList
         |> List.mapi (fun y l -> l |> List.mapi (fun x c -> (x,y), c |> string |> String.parseToIntX))
         |> pso "LL:  "
         |> List.concat
         |> List.toMap
         |> pso "\n\nMap: "

      let printMap (map : Map<int * int, _>) =

         let maxX =
            map
            |> Map.toSetOfKeys
            |> Set.map fst
            |> Set.maxElement

         let maxY =
            map
            |> Map.toSetOfKeys
            |> Set.map snd
            |> Set.maxElement

         for y in [0 .. maxY] do
            print "\n"
            for x in [0 .. maxX] do
               let v = Map.find (x,y) map
               let v2 = if v then '1' else '0'
               print $"{v2}"

      let findDimensions map =
         let maxX =
            map
            |> Map.toSetOfKeys
            |> Set.map fst
            |> Set.maxElement

         let maxY =
            map
            |> Map.toSetOfKeys
            |> Set.map snd
            |> Set.maxElement

         (maxX, maxY)

      let lookRight map  =
         let maxX, maxY = findDimensions map

         [ for y in [0 .. maxY] do
            let mutable maxHeight = -1
            [ for x in [0 .. maxX] do
               let height = Map.find (x,y) map
               if height > maxHeight then
                  yield (x,y), true
                  maxHeight <- height
               else
                  yield (x,y), false
                  ] ]
         |> List.concat
         |> List.toMap

      let lookLeft map =
         let maxX, maxY = findDimensions map
         [ for y in [0 .. maxY] do
            let mutable maxHeight = -1
            [ for x in [maxX..(-1)..0] do
               let height = Map.find (x,y) map
               if height > maxHeight then
                  yield (x,y), true
                  maxHeight <- height
               else
                  yield (x,y), false
                  ] ]
         |> List.concat
         |> List.toMap

      let lookDown map =
         let maxX, maxY = findDimensions map
         [ for x in [0 .. maxX] do
            let mutable maxHeight = -1
            [ for y in [0..maxY] do
               let height = Map.find (x,y) map
               if height > maxHeight then
                  yield (x,y), true
                  maxHeight <- height
               else
                  yield (x,y), false
                  ] ]
         |> List.concat
         |> List.toMap

      let lookUp map =
         let maxX, maxY = findDimensions map
         [ for x in [0 .. maxX] do
            let mutable maxHeight = -1
            [ for y in [maxY.. -1..0] do
               let height = Map.find (x,y) map
               if height > maxHeight then
                  yield (x,y), true
                  maxHeight <- height
               else
                  yield (x,y), false
                  ] ]
         |> List.concat
         |> List.toMap

      let findVisible (map : Map<int * int, int>) =
         let maxX, maxY = findDimensions map

         // scan from left
         let left = lookRight map
         let right = lookLeft map
         let top = lookDown map
         let bottom = lookUp map

         let combined =
            [ for x in [0 .. maxX] do
               [ for y in [maxY.. -1..0] do
                  let v =
                     Map.find (x,y) top = true
                     || Map.find (x,y) bottom = true
                     || Map.find (x,y) left = true
                     || Map.find (x,y) right = true

                  yield (x,y), v
                     ] ]
            |> List.concat
            |> List.toMap

         let count =
            combined
            |> Map.toList
            |> List.map snd
            |> List.countBy id
            |> pso "Count: "

         ()



      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse
         |> findVisible

   module Part2 =

      let parse (lines : seq<string>) =
         lines
         |> Seq.toList
         |> List.map Seq.toList
         |> List.mapi (fun y l -> l |> List.mapi (fun x c -> (x,y), c |> string |> String.parseToIntX))
         |> pso "LL:  "
         |> List.concat
         |> List.toMap
         |> pso "\n\nMap: "

      let printMap (map : Map<int * int, _>) =

         let maxX =
            map
            |> Map.toSetOfKeys
            |> Set.map fst
            |> Set.maxElement

         let maxY =
            map
            |> Map.toSetOfKeys
            |> Set.map snd
            |> Set.maxElement

         for y in [0 .. maxY] do
            for x in [0 .. maxX] do
               let v = Map.find (x,y) map
               let v2 = if v then '1' else '0'
               print $"{v2}"

      let findDimensions map =
         let maxX =
            map
            |> Map.toSetOfKeys
            |> Set.map fst
            |> Set.maxElement

         let maxY =
            map
            |> Map.toSetOfKeys
            |> Set.map snd
            |> Set.maxElement

         (maxX, maxY)

      let lookRightFrom (startX, startY) map  =
         let maxX, _maxY = findDimensions map
         let y = startY
         [ for x in [startX + 1 ..maxX] do
               yield (x,y), (Map.find (x,y) map)
         ]

      let lookLeftFrom (startX, startY) map =
         let y = startY
         [ for x in [startX - 1 .. -1 .. 0] do
               yield (x,y), (Map.find (x,y) map)
         ]

      let lookDownFrom (startX, startY) map =
         let _maxX, maxY = findDimensions map
         let x = startX
         [ for y in [startY + 1 .. 1.. maxY] do
               yield (x,y), (Map.find (x,y) map)
         ]

      let lookUpFrom (startX, startY) (map : Map<int * int, int>) =
         let x = startX
         [ for y in [startY - 1 .. -1 .. 0] do
               yield (x,y), (Map.find (x,y) map)
         ]

      let countView houseHeight (view : List<(int * int) * int>)  =

         let rec inner count l =
            match l with
            | h :: hs ->
               if h < houseHeight then inner (count + 1) hs else count + 1
            | [] -> count

         view |> List.map snd |> inner 0


      let findVisible (map : Map<int * int, int>) =
         let maxX, maxY = findDimensions map

         [ for x in [0 .. maxX] do
              for y in [0 .. maxY] do
                 let houseHeight = Map.find (x,y) map
                 let up =
                    lookUpFrom (x,y) map
                 let left =
                    lookLeftFrom (x,y) map

                 let down =
                    lookDownFrom (x,y) map
                 let right =
                    lookRightFrom (x,y) map

                 [ up; left; down; right ]
                 //|> List.map (List.map snd)
                 |> List.map (countView houseHeight)
                 |> id

                 //printfn $"({(x,y)})  ({(up,left,down,right)})"

         ] : List<List<int>>
         |> List.map (List.reduce ( * ))
         //|> pso "Scores: "
         |> List.maxBy id
         |> id

      let go (year, day) runMode =
         getInput (year, day) runMode
         |> parse
         |> findVisible

   let run (year, day) =
      // Full |> Part1.go (year, day) |> printn
      Full |> Part2.go (year, day) |> printn
