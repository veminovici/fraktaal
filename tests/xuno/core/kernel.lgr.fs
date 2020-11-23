namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems
open Simplee.DSystems.Kernel

type LoggerTests(output: ITestOutputHelper) =

    [<Fact>]
    let ``Create logger`` () =

        let (lgr, trace) = Logger.create ()

        async {
            do! 
                "test"
                |> Trace.papi (ProcessId.ofStr "pid") (SessionId.ofStr "sid")
                |> List.ofItem
                |> trace
        }
        |> Async.sleep 100
        |> Async.RunSynchronously

        lgr.Logs
        |> Async.RunSynchronously
        |> List.length
        |> (<>) 0
        |> Assert.True