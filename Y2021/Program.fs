
  // Define a function to construct a message to print
  let from whom =
      sprintf "from %s" whom

  [<EntryPoint>]
  let main argv =
      AdventOfCode.Y2021.Day08B.run ()
      0
