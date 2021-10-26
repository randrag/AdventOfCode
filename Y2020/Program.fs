namespace AdventOfCode


module Main =
  [<EntryPoint>]
  let main _ =

      let stopWatch = System.Diagnostics.Stopwatch.StartNew()

      do Day18.run ()

      printf "\n\n\n"
      printfn "That took %f ms" stopWatch.Elapsed.TotalMilliseconds
      stopWatch.Stop()
      0 // return an integer exit code
