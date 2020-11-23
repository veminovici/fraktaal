namespace Testly.DSystems.Kernel

module Payload = 

    open System
    open Xunit

    open Simplee.DSystems
    open System.Text.Encodings

    let mySerializer = { new ISerializer with
        member _.Serialize a = [|0uy; 1uy|] 
        member _.Deserialize<'a> bs = Unchecked.defaultof<'a> }


    [<Fact>]
    let ``Payload of bytes`` () =

        let xs = [|0uy; 1uy|]

        xs
        |> Payload.ofBytes
        |> function
        | Payload bs -> 
            let f = xs = bs
            Assert.True f
        | _          ->
            Assert.True false

    [<Fact>]
    let ``Empty Payload`` () =

        Payload.empty
        |> function
        | Payload bs -> 
            let f = Array.empty = bs
            Assert.True f
        | _ ->
            Assert.True false

    [<Fact>]
    let ``Payload of Item`` () =

        let x = "test"

        x
        |> Payload.ofItem mySerializer
        |> function
        | Payload bs -> 
            let f = Array.length bs = 2
            Assert.True f
        | _ ->
            Assert.True false

    [<Fact>]
    let ``Payload to Item`` () =

        let x = "test"

        let f =
            x
            |> Payload.ofItem mySerializer
            |> Payload.toItem mySerializer
            |> (=) ""

        Assert.True f
