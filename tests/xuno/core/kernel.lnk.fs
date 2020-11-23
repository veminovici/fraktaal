namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems
open Simplee.DSystems.Link.Ops

type LinkTests(output: ITestOutputHelper) =

    [<Fact>]
    let ``Unidirectional link`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        pid0 
        =>> pid1
        |> function
        | OneWay  ldt -> 
            ldt |> LinkData.fid       |> (=) pid0        |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1        |> Assert.True
            ldt |> LinkData.weight    |> (=) NoWeight    |> Assert.True
            ldt |> LinkData.direction |> (=) NoDirection |> Assert.True
        | TwoWays _ -> Assert.True false

    [<Fact>]
    let ``Bidirectional link`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        pid0 
        <=> pid1
        |> function
        | OneWay  _ -> 
            Assert.True false
        | TwoWays ldt -> 
            ldt |> LinkData.fid       |> (=) pid0        |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1        |> Assert.True
            ldt |> LinkData.weight    |> (=) NoWeight    |> Assert.True
            ldt |> LinkData.direction |> (=) NoDirection |> Assert.True

    [<Fact>]
    let ``Unidirectional link from left to right`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        pid0 
        .>> pid1
        |> function
        | OneWay  ldt -> 
            ldt |> LinkData.fid       |> (=) pid0             |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1             |> Assert.True
            ldt |> LinkData.weight    |> (=) NoWeight         |> Assert.True
            ldt |> LinkData.direction |> (=) ToRightDirection |> Assert.True
        | TwoWays _ -> Assert.True false

    [<Fact>]
    let ``Unidirectional link from right to left`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        pid1 
        <<. pid0
        |> function
        | OneWay  ldt -> 
            ldt |> LinkData.fid       |> (=) pid0            |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1            |> Assert.True
            ldt |> LinkData.weight    |> (=) NoWeight        |> Assert.True
            ldt |> LinkData.direction |> (=) ToLeftDirection |> Assert.True
        | TwoWays _ -> Assert.True false

    [<Fact>]
    let ``Unidirectional link with weight`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        (pid0 
        =>> pid1)
        ++ 1.0
        |> function
        | OneWay  ldt -> 
            ldt |> LinkData.fid       |> (=) pid0         |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1         |> Assert.True
            ldt |> LinkData.weight    |> (=) (Weight 1.0) |> Assert.True
            ldt |> LinkData.direction |> (=) NoDirection  |> Assert.True
        | TwoWays _ -> Assert.True false

    [<Fact>]
    let ``Unidirectional link with direction`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        pid0 
        =>> pid1
        |> Link.withDirection ToRightDirection
        |> function
        | OneWay  ldt -> 
            ldt |> LinkData.fid       |> (=) pid0             |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1             |> Assert.True
            ldt |> LinkData.weight    |> (=) NoWeight         |> Assert.True
            ldt |> LinkData.direction |> (=) ToRightDirection |> Assert.True
        | TwoWays _ -> Assert.True false


    [<Fact>]
    let ``Bidirectional link with weight`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        (pid0 
        <=> pid1)
        ++ 1.0
        |> function
        | OneWay  _ -> 
            Assert.True false
        | TwoWays ldt -> 
            ldt |> LinkData.fid       |> (=) pid0         |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1         |> Assert.True
            ldt |> LinkData.weight    |> (=) (Weight 1.0) |> Assert.True
            ldt |> LinkData.direction |> (=) NoDirection  |> Assert.True

    [<Fact>]
    let ``Bidirectional link with direction`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        (pid0 
        <=> pid1)
        |> Link.withDirection ToLeftDirection
        |> function
        | OneWay  _ -> 
            Assert.True false
        | TwoWays ldt -> 
            ldt |> LinkData.fid       |> (=) pid0            |> Assert.True 
            ldt |> LinkData.tid       |> (=) pid1            |> Assert.True
            ldt |> LinkData.weight    |> (=) NoWeight        |> Assert.True
            ldt |> LinkData.direction |> (=) ToLeftDirection |> Assert.True

    [<Fact>]
    let ``Neighbors from bidirectional link`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        let s, e = pid0 <=> pid1 |> Neighbor.endsOfLink

        s 
        |> fst 
        |> FromId.toProcessId 
        |> (=) pid0 
        |> Assert.True

        s 
        |> snd
        |> function
        | Neighbor.SrcRcvNeighbor (p, w, d) -> 
            p |> (=) pid1        |> Assert.True
            d |> (=) NoDirection |> Assert.True
            w |> (=) NoWeight    |> Assert.True
        | _ -> 
            Assert.True false

        e 
        |> fst 
        |> ToId.toProcessId
        |> (=) pid1
        |> Assert.True

        e
        |> snd
        |> function
        | Neighbor.SrcRcvNeighbor (p, w , d) ->
            p |> (=) pid0        |> Assert.True
            d |> (=) NoDirection |> Assert.True
            w |> (=) NoWeight    |> Assert.True
        | _ -> 
            Assert.True false

    [<Fact>]
    let ``Neighbors from unidirectional link`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"

        let s, e = pid0 =>> pid1 |> Neighbor.endsOfLink

        s 
        |> fst 
        |> FromId.toProcessId 
        |> (=) pid0 
        |> Assert.True


        s 
        |> snd
        |> function
        | Neighbor.RcvNeighbor (p, w, d) -> 
            p |> (=) pid1        |> Assert.True
            d |> (=) NoDirection |> Assert.True
            w |> (=) NoWeight    |> Assert.True
        | _ -> 
            Assert.True false

        e 
        |> fst 
        |> ToId.toProcessId
        |> (=) pid1
        |> Assert.True

        e
        |> snd
        |> function
        | Neighbor.SrcNeighbor (p, w , d) ->
            p |> (=) pid0        |> Assert.True
            d |> (=) NoDirection |> Assert.True
            w |> (=) NoWeight    |> Assert.True
        | _ -> 
            Assert.True false

    [<Fact>]
    let ``Weight to string`` () =
        NoWeight
        |> sprintf "%O"
        |> String.length
        |> (<>) 0
        |> Assert.True

        1.0
        |> Weight
        |> sprintf "%O"
        |> String.length
        |> (<>) 0
        |> Assert.True

    [<Fact>]
    let ``Neighbor to string`` () =

        Sourcer (ProcessId.ofStr "pid", NoWeight, NoDirection)
        |> sprintf "%O"
        |> String.length
        |> (<>) 0
        |> Assert.True

        Receiver(ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> sprintf "%O"
        |> String.length
        |> (<>) 0
        |> Assert.True

        SourcerOrReceiver (ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> sprintf "%O"
        |> String.length
        |> (<>) 0
        |> Assert.True

    [<Fact>]
    let ``Neighbor to string with title`` () =

        [
        Sourcer (ProcessId.ofStr "pid", NoWeight, NoDirection)
        ]
        |> Neighbor.toStringWithTtl "ttl" (ProcessId.ofStr "pid")
        |> String.length
        |> (<>) 0
        |> Assert.True

    [<Fact>]
    let ``Neighbor pid`` () =
        Sourcer (ProcessId.ofStr "pid", NoWeight, NoDirection)
        |> Neighbor.pid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

        Receiver(ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> Neighbor.pid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

        SourcerOrReceiver (ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> Neighbor.pid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

    [<Fact>]
    let ``Neighbor fid`` () =
        Sourcer (ProcessId.ofStr "pid", NoWeight, NoDirection)
        |> Neighbor.fid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

        Receiver(ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> Neighbor.fid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

        SourcerOrReceiver (ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> Neighbor.fid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

    [<Fact>]
    let ``Neighbor tid`` () =
        Sourcer (ProcessId.ofStr "pid", NoWeight, NoDirection)
        |> Neighbor.tid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

        Receiver(ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> Neighbor.tid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

        SourcerOrReceiver (ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> Neighbor.tid
        |> sprintf "%O"
        |> (=) "pid"
        |> Assert.True

    [<Fact>]
    let ``Neighbor can send to`` () =
        Sourcer (ProcessId.ofStr "pid", NoWeight, NoDirection)
        |> function
        | Neighbor.CanSendTo _ -> Assert.True false
        | _                    -> Assert.True true

        Receiver(ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> function
        | Neighbor.CanSendTo _ -> Assert.True true
        | _                    -> Assert.True false

        SourcerOrReceiver (ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> function
        | Neighbor.CanSendTo _ -> Assert.True true
        | _                    -> Assert.True false

    [<Fact>]
    let ``Neighbor can receive from`` () =
        Sourcer (ProcessId.ofStr "pid", NoWeight, NoDirection)
        |> function
        | Neighbor.CanReceiveFrom _ -> Assert.True true
        | _                         -> Assert.True false

        Receiver(ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> function
        | Neighbor.CanReceiveFrom _ -> Assert.True false
        | _                         -> Assert.True true

        SourcerOrReceiver (ProcessId.ofStr "pid", Weight 1.0, ToLeftDirection)
        |> function
        | Neighbor.CanReceiveFrom _ -> Assert.True true
        | _                         -> Assert.True false
