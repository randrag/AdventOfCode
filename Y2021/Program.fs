
  // Define a function to construct a message to print
  let from whom =
      sprintf "from %s" whom

  [<EntryPoint>]
  let main argv =
      AdventOfCode.Y2015.Day03.Part2.run ()
      0
