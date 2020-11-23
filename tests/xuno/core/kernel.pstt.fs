namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems
open Simplee.DSystems.Link.Ops

type ProcessStateTests(output: ITestOutputHelper) =

    [<Fact>]
    let ``From data back to data`` () =

        let x = 10

        x
        |> ProcessState.ofItem
        |> ProcessState.toItem
        |> (=) x
        |> Assert.True