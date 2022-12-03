namespace AdventOfCode
open FsToolkit.ErrorHandling

open Helpers

module Day17 =
  module Part1 =
    let read fileName  = System.IO.File.ReadLines($"/Users/roland/Code/AdventOfCode/Y2020/{fileName}.txt")

    type Point = int * int * int
    type Space = Map<Point,int>

    let parseChar ((x, y, z) : Point) space c : Space =
      //ps "   enter parseChar: " ((x, y, z), c)
      match c with
      | '#' -> Map.add (x, y, z) 1 space
      | _   -> Map.add (x, y, z) 0 space

    let rec parseLine (x, y, z) space line : Space =
      //ps " enter parseLine: " ((x, y, z), line)
      match Seq.tryHead line with
      | Some c ->
          let newSpace = parseChar (x, y, z) space c
          parseLine (x + 1, y, z) newSpace (Seq.tail line)
      | None -> space

    let rec parseInput (x, y, z) space (input : seq<string>) =
      //ps "enter parseInput: " ((x, y, z), input)
      match Seq.tryHead input with
      | Some line ->
          let newSpace = parseLine (x, y, z) space line
          parseInput (x, y - 1, z) newSpace (Seq.tail input)
      | None -> space

    let getAllPointsInSpace (space : Space) =
        space
        |> Map.toList
        |> List.map (fun ((x, y, z), _) -> (x, y, z))

    let getExtents ( points : List<Point> ) =
      let (xe, ye, ze) = List.unzip3 points
      ((List.min xe, List.max xe), (List.min ye, List.max ye), (List.min ze, List.max ze))

    let getAllCoordsInCube ((minX, maxX),(minY, maxY),(minZ, maxZ)) =
      seq { for x in [minX .. 1 .. maxX] do
              for y in [minY .. 1 .. maxY] do
                for z in [minZ .. 1 .. maxZ] do
                   yield (x, y, z) }

    let enlargeExtents ((minX, maxX),(minY, maxY),(minZ, maxZ)) =
      ((minX - 1, maxX + 1), (minY - 1, maxY + 1), (minZ - 1, maxZ + 1))

    let getComplimentCoords large small =
      seq {
        for point in large do
          if not (Seq.contains point small) then yield point }

    let getPointsAroundCube cubeSize =
      let CoordsInCube = getAllCoordsInCube cubeSize
      let CoordsInLargerCube = cubeSize |> enlargeExtents |> getAllCoordsInCube
      let newCoords = getComplimentCoords CoordsInLargerCube CoordsInCube
      newCoords

    let getCoordsAroundPoint (point : Point) =
      let (x,y,z) = point
      seq { for x' in [x - 1 .. 1 .. x + 1] do
              for y' in [y - 1 .. 1 .. y + 1] do
                for z' in [z - 1 .. 1 .. z + 1] do
                   yield (x', y', z') }
      |> Seq.filter ((<>) point)

    let addInactivePointToSpace (space : Space) (point : Point) =
      Map.add point 0 space

    let addInactivePointsToSpace (space : Space)  (points : seq<Point>) =
      points |> Seq.fold addInactivePointToSpace space

    let enlargeSpace space : Space =
      space |> getAllPointsInSpace |> getExtents |> getPointsAroundCube |> addInactivePointsToSpace space

    let getActiveCount (space : Space) point =
      point
      |> getCoordsAroundPoint
      |> Seq.map (fun point -> Map.tryFind point space |> Option.defaultValue 0)
      |> Seq.sum

    let nextState currentState activeCount =
      match currentState, activeCount with
      | 1, 2
      | 1, 3 -> 1
      | 1, _ -> 0
      | 0, 3 -> 1
      | 0, _ -> 0
      | _ -> failwith "Never"

    let sprintSpace (space : Space) =
      let ((minX, maxX), (minY, maxY), (minZ, maxZ))  = space |> getAllPointsInSpace |> getExtents |> pso "extents: "
      seq { yield '\n'
            for z in [minZ .. 1 .. maxZ] do
              for y in [maxY .. -1 .. minY] do
                for x in [minX .. 1 .. maxX] do
                  if Map.find (x,y,z) space   = 1 then yield '#' else yield '.'

                yield '\n'
              yield '\n' } |> System.String.Concat


    let step space : Space =
      let largerSpace = enlargeSpace space
      //do printf "!larger space: \n%s" (sprintSpace largerSpace)

      largerSpace
      |> Map.map (fun point currentState ->
          point
          |> getActiveCount largerSpace
          //|> fun activeCount -> printf "current state, active count: %A" (currentState, activeCount, point ); activeCount
          |> nextState currentState )




    let run () =
      let input = read "input17"
      let space = parseInput (0, 0, 0) Map.empty input |> pso "input space: "
      do sprintSpace space |> ps "sprintSpace: \n"
      let extents = space |> getAllPointsInSpace |> getExtents  |> pso "extents: "
      let newCoords = getPointsAroundCube extents |> Seq.toList |> pso "larger cube: "
      let count = List.length newCoords |> pso "count: "
      let extents2 = getExtents newCoords |> pso "larger extents"
      let t = getPointsAroundCube ((0,0), (0,0), (0,0)) |> Seq.toList |> pso "points around 0,0,0: "
      do step space
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
           |> Map.toList
         |> List.sumBy (fun (_, value) -> value)
         |> ps "Total active: "

      ()
  module Part2 =
    let read fileName  = System.IO.File.ReadLines($"/Users/roland/Code/AdventOfCode/Y2020/{fileName}.txt")

    type Point = int * int * int * int
    type Space = Map<Point,int>

    let parseChar ((x, y, z, w) : Point) space c : Space =
      //ps "   enter parseChar: " ((x, y, z), c)
      match c with
      | '#' -> Map.add (x, y, z, w) 1 space
      | _   -> Map.add (x, y, z, w) 0 space

    let rec parseLine (x, y, z, w) space line : Space =
      //ps " enter parseLine: " ((x, y, z), line)
      match Seq.tryHead line with
      | Some c ->
          let newSpace = parseChar (x, y, z, w) space c
          parseLine (x + 1, y, z, w) newSpace (Seq.tail line)
      | None -> space

    let rec parseInput (x, y, z, w) space (input : seq<string>) =
      //ps "enter parseInput: " ((x, y, z), input)
      match Seq.tryHead input with
      | Some line ->
          let newSpace = parseLine (x, y, z, w) space line
          parseInput (x, y - 1, z, w) newSpace (Seq.tail input)
      | None -> space

    let getAllPointsInSpace (space : Space) =
        space
        |> Map.toList
        |> List.map (fun ((x, y, z, w), _) -> (x, y, z, w))

    let getExtents ( points : List<Point> ) =
      let (xe, ye, ze, we) = List.unzip4 points
      ((List.min xe, List.max xe)
       , (List.min ye, List.max ye)
       , (List.min ze, List.max ze)
       , (List.min we, List.max we))

    let getAllCoordsInCube ((minX, maxX),(minY, maxY),(minZ, maxZ),(minW, maxW)) =
      seq { for w in [minW .. 1 .. maxW] do
              for x in [minX .. 1 .. maxX] do
                for y in [minY .. 1 .. maxY] do
                  for z in [minZ .. 1 .. maxZ] do
                     yield (x, y, z, w) }

    let enlargeExtents ((minX, maxX),(minY, maxY),(minZ, maxZ),(minW, maxW)) =
      ((minX - 1, maxX + 1), (minY - 1, maxY + 1), (minZ - 1, maxZ + 1), (minW - 1, maxW + 1))

    let getComplimentCoords large small =
      seq {
        for point in large do
          if not (Seq.contains point small) then yield point }

    let getPointsAroundCube cubeSize =
      let CoordsInCube = getAllCoordsInCube cubeSize
      let CoordsInLargerCube = cubeSize |> enlargeExtents |> getAllCoordsInCube
      let newCoords = getComplimentCoords CoordsInLargerCube CoordsInCube
      newCoords

    let getCoordsAroundPoint (point : Point) =
      let (x,y,z,w) = point
      seq { for w' in [w - 1 .. 1 .. w + 1] do
              for x' in [x - 1 .. 1 .. x + 1] do
                for y' in [y - 1 .. 1 .. y + 1] do
                  for z' in [z - 1 .. 1 .. z + 1] do
                     yield (x', y', z', w') }
      |> Seq.filter ((<>) point)

    let addInactivePointToSpace (space : Space) (point : Point) =
      Map.add point 0 space

    let addInactivePointsToSpace (space : Space)  (points : seq<Point>) =
      points |> Seq.fold addInactivePointToSpace space

    let enlargeSpace space : Space =
      space |> getAllPointsInSpace |> getExtents |> getPointsAroundCube |> addInactivePointsToSpace space

    let getActiveCount (space : Space) point =
      point
      |> getCoordsAroundPoint
      |> Seq.map (fun point -> Map.tryFind point space |> Option.defaultValue 0)
      |> Seq.sum

    let nextState currentState activeCount =
      match currentState, activeCount with
      | 1, 2
      | 1, 3 -> 1
      | 1, _ -> 0
      | 0, 3 -> 1
      | 0, _ -> 0
      | _ -> failwith "Never"

    let sprintSpace (space : Space) =
      let ((minX, maxX), (minY, maxY), (minZ, maxZ), (minW, maxW))  = space |> getAllPointsInSpace |> getExtents |> pso "extents: "
      seq { yield '\n'
            for w in [minW .. 1 .. maxW] do
              for z in [minZ .. 1 .. maxZ] do
                yield! (sprintf $"z = {z}, w = {w};\n")
                for y in [maxY .. -1 .. minY] do
                  for x in [minX .. 1 .. maxX] do
                    yield if Map.find (x,y,z,w) space = 1 then '#' else '.'
                  yield '\n'
                yield '\n' }
      |> System.String.Concat


    let step space : Space =
      let largerSpace = enlargeSpace space
      //do printf "!larger space: \n%s" (sprintSpace largerSpace)

      largerSpace
      |> Map.map (fun point currentState ->
          point
          |> getActiveCount largerSpace
          //|> fun activeCount -> printf "current state, active count: %A" (currentState, activeCount, point ); activeCount
          |> nextState currentState )




    let run () =
      let input = read "input17"
      let space = parseInput (0, 0, 0, 0) Map.empty input |> pso "input space: "
      do sprintSpace space |> ps "sprintSpace: \n"
      let extents = space |> getAllPointsInSpace |> getExtents  |> pso "extents: "
      let newCoords = getPointsAroundCube extents |> Seq.toList |> pso "larger cube: "
      let count = List.length newCoords |> pso "count: "
      let extents2 = getExtents newCoords |> pso "larger extents"
      let t = getPointsAroundCube ((0,0), (0,0), (0,0), (0,0)) |> Seq.toList |> pso "points around 0,0,0: "
      do step space
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
         |> step
         |> fun space -> printf "%s" (sprintSpace space); space
           |> Map.toList
         |> List.sumBy (fun (_, value) -> value)
         |> ps "Total active: "

  let run () = Part2.run()
