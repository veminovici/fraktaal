namespace Testly.DSystems.Kernel

module Async = 

    open System
    open Xunit

    open Simplee.DSystems

    [<Fact>]
    let ``Async map`` () =

        let x = async.Return "test"
        let len = x |> Async.map (String.length) |> Async.RunSynchronously
        let f = len = 4

        Assert.True f

    [<Fact>]
    let ``Async apply`` () =

        let x = async.Return "test"
        let f = async.Return String.length
        let len = Async.apply f x |> Async.RunSynchronously
        let f = len = 4

        Assert.True f

    [<Fact>]
    let ``Async sleep`` () =

        let x = async.Return "test"
        let len = x |> Async.map String.length |> Async.sleep 1 |> Async.RunSynchronously
        let f = len = 4

        Assert.True f