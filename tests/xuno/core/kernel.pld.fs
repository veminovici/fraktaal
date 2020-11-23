namespace Testly.DSystems.Kernel


open Xunit
open Xunit.Abstractions

open Simplee.DSystems

type PayloadTests(output: ITestOutputHelper) =

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

    let ``Payload to string`` () =

        let xs = [|0uy; 1uy|]

        xs 
        |> Payload.ofBytes
        |> sprintf "%O"
        |> String.length
        |> (<>) 0
        |> Assert.True

        PingPayload
        |> sprintf "%O"
        |> String.length
        |> (<>) 0
        |> Assert.True

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

        let x': string =
            x
            |> Payload.ofItem mySerializer
            |> Payload.toItem mySerializer

        let f = x' = Unchecked.defaultof<string>
        output.WriteLine <|  sprintf "Result [%O] test=%b" x' f

        Assert.True f
