namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems
open Simplee.DSystems.Link.Ops

type ArgTests(output: ITestOutputHelper) =

    [<Fact>]
    let ``From data back to data`` () =

        let x = 10

        x
        |> ApiArguments.ofItem
        |> ApiArguments.toItem
        |> (=) x
        |> Assert.True