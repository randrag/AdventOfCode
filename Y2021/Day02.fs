namespace AdventOfCode.Y2015

open AdventOfCode.Parsing

module Day02 =
  let loadLines () = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2015/input02.txt")

  let parseLine (s : string) =
      s.Split('x')
      |> Array.toList
      |> function
          | [IntX l; IntX w; IntX h] -> (l, w, h)
          | _ -> failwith "Unexpected input"

  module Part1 =
    let calculateBoxArea (l, w, h) = 2*l*w + 2*w*h + 2*h*l
    let calculateAreaOfSmallestSide (l, w, h) = [l*w; w*h; h*l] |> List.min
    let calculateRequiredPaper (l, w, h) = calculateBoxArea (l, w, h) + calculateAreaOfSmallestSide (l, w, h)
    let run () =  loadLines () |> Seq.sumBy (parseLine >> calculateRequiredPaper)

  module Part2 =
    let calculateSmallestCircumference (l, w, h) = [2*l + 2*w; 2*l + 2*h; 2*w + 2*h] |> List.min
    let calculateVolume (l, w, h) = l * w * h
    let calculateRibbonLength (l, w, h) = calculateSmallestCircumference (l, w, h) + calculateVolume (l, w, h)
    let run () = loadLines () |> Seq.sumBy (parseLine >> calculateRibbonLength)


  let run () =
    printfn $"Part1 {(Part1.run())}"
    printfn $"Part2 {(Part2.run())}"
