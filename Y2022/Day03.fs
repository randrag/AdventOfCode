namespace AdventOfCode.Y2022

open Helpers


module Day03 =

   type System.Char with
      member c.toInt = int c



   let score (char : char) =
      if ('a').toInt <= char.toInt && char.toInt <= ('z').toInt
      then
         char.toInt - ('a').toInt + 1
      else
         char.toInt - 'A'.toInt + 27


   let getInput () =
      System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2022/input03.txt")






   module Part1 =
      let splitList (l : List<_>) =
        let length = List.length l
        let h1 = l |> List.take (length/2)
        let h2 = l |> List.skip (length/2)
        h1, h2


      let go () =
         getInput ()
         |> Seq.toList
         |> List.map (fun string ->
               string
               |> Seq.toList
               |> splitList
               |> fun (a, b) -> (
                     Set.intersect (a |> List.toSet) (b |> List.toSet)))
         |> List.collect (Set.map score >> Set.toList)
         |> List.sum

         |> fun x -> printfn $"%A{x}"

   module Part2 =

      let go () =
         getInput ()
         |> Seq.toList
         |> Part1.splitList
         |> fun (l1, l2) ->
               let set1 =
                  l1
                  |> List.map (fun l ->

                           l
                           |> Seq.toList
                           |> pso "Set1: "
                           |> List.toSet
                        )
                  |> id
                  |> List.reduce Set.intersect
               let set2 =
                  l2
                  |> List.map (fun l ->

                           l
                           |> Seq.toList
                           |> pso "Set2: "
                           |> List.toSet
                        )
                  |> id
                  |> List.reduce Set.intersect
               List.append (set1 |> Set.toList) (set2 |> Set.toList)
               |> fun x -> printfn $"%A{x}"; x
               |> List.sumBy score

         |> fun x -> printfn $"%A{x}"

   module Part2b =

      let rec divideList (l1 : List<_>) (acc : List<List<_>>) =
         match l1 with
         | [] -> acc |> List.rev |> List.tail
         | nonEmptyList ->
            divideList (nonEmptyList |> List.skip 3) ((nonEmptyList |> List.take 3)::acc)

      let go () =
         getInput ()
         |> Seq.toList
         // divide into groups of three
         |> fun l -> divideList l [[]]
         |> pso "Divided list"
         |> List.map (fun l -> // list of three strings
                  l
                  |> List.map Seq.toSet
                  |> List.reduce Set.intersect
               )
         |> pso "Intersections: "
         |> List.sumBy (fun s -> s |> Set.toList |> List.sumBy score)

//         |> fun (l1, l2) ->
//               let set1 =
//                  l1
//                  |> List.map (fun l ->
//
//                           l
//                           |> Seq.toList
//                           |> pso "Set1: "
//                           |> List.toSet
//                        )
//                  |> id
//                  |> List.reduce Set.intersect
//               let set2 =
//                  l2
//                  |> List.map (fun l ->
//
//                           l
//                           |> Seq.toList
//                           |> pso "Set2: "
//                           |> List.toSet
//                        )
//                  |> id
//                  |> List.reduce Set.intersect
//               List.append (set1 |> Set.toList) (set2 |> Set.toList)
//               |> fun x -> printfn $"%A{x}"; x
//               |> List.sumBy score
//
//         |> fun x -> printfn $"%A{x}"
//



   let run () =
      //Part1.go () |> p
      Part2b.go () |> p
