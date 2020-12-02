// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    //printfn "%A" (Day1.part1b Day1.inputData)
    //printfn "%A" (Day1.part2b Day1.inputData)

    printfn "%A" Day2.Part1.part1

    printfn "That took %f ms" stopWatch.Elapsed.TotalMilliseconds
    printfn "%A" Day2.Part1.part2

    stopWatch.Stop()
    printfn "That took %f ms" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code
