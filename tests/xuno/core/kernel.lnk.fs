namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems
open Simplee.DSystems.Link.Ops

type LinkTests(output: ITestOutputHelper) =

    [<Fact>]
    let ``One way link`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        let lnk = pid0 =>> pid1
        match lnk with
        | OneWay  _ -> Assert.True(true)
        | TwoWays _ -> Assert.True(false)

    [<Fact>]
    let ``Two ways link`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        let lnk = pid0 <=> pid1
        match lnk with
        | OneWay  _ -> Assert.True false
        | TwoWays _ -> Assert.True true
