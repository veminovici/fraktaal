namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems
open Simplee.DSystems.Link.Ops

type ApiResultTests(output: ITestOutputHelper) =

    [<Fact>]
    let ``From data back to data`` () =

        let x = 10

        x
        |> ApiResult.ofItem
        |> ApiResult.toItem
        |> (=) x
        |> Assert.True

    [<Fact>]
    let ``ApiResult from unit`` () =
        ApiResult.nil
        |> ApiResult.toItem
        |> (=) ()
        |> Assert.True
