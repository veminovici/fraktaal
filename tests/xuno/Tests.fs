module Tests

open System
open Xunit

open Simplee.DSystems
open Simplee.DSystems.Link.Ops

[<Fact>]
let ``My test`` () =
    let pid0 = ProcessId.ofStr "p0"
    let pid1 = ProcessId.ofStr "p1"

    let lnk = pid0 =>> pid1
    match lnk with
    | OneWay  _ -> Assert.True(true)
    | TwoWays _ -> Assert.True(false)
