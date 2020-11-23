namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems

type EnvelopeTests(output: ITestOutputHelper) =

    let mySerializer = { new ISerializer with
        member _.Serialize a = [|0uy; 1uy|] 
        member _.Deserialize<'a> bs = Unchecked.defaultof<'a> }

    [<Fact>]
    let ``Envelope of rnd eid`` () =

        let env = 
            ()
            |> Envelope.ofRndEid
            |> Envelope.withSid (SessionId.ofStr "sid")
            |> Envelope.withFid (FromId.ofStr "fid")
            |> Envelope.withTid (ToId.ofStr "tid")

        Assert.True (Envelope.eid env <> EnvelopeId.empty)
        Assert.True (env |> Envelope.sid |> SessionId.sid |> (=) "sid")
        Assert.True (env |> Envelope.fid |> FromId.fid |> (=) "fid")
        Assert.True (env |> Envelope.tid |> ToId.tid |> (=) "tid")

    [<Fact>]
    let ``Envelope fields`` () =

        let env = 
            Envelope.empty
            |> Envelope.withRndEid
            |> Envelope.withSid (SessionId.ofStr "sid")
            |> Envelope.withFid (FromId.ofStr "fid")
            |> Envelope.withTid (ToId.ofStr "tid")

        Assert.True (Envelope.eid env <> EnvelopeId.empty)
        Assert.True (env |> Envelope.sid |> SessionId.sid |> (=) "sid")
        Assert.True (env |> Envelope.fid |> FromId.fid |> (=) "fid")
        Assert.True (env |> Envelope.tid |> ToId.tid |> (=) "tid")
