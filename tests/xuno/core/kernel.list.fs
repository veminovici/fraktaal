namespace Testly.DSystems.Kernel

module TstList = 

    open System
    open Xunit

    open Simplee.DSystems

    [<Fact>]
    let ``List of one item`` () =

        let x = "test"
        let lst = List.ofItem x

        Assert.True (List.length lst = 1)
        Assert.True (List.head lst = x)

    [<Fact>]
    let ``Sublist`` () =

        let xs = [1;2;3;4;5]
        let ys = [2;3;5]

        Assert.True  (List.sublistOf xs ys)
        Assert.True  (List.sublistOf xs [])
        Assert.False (List.sublistOf ys xs)

    [<Fact>]
    let ``To string`` () =

        let xs = [2;3]
        let str = List.toString xs
        let f = str = "2\n3"

        Assert.True f

    [<Fact>]
    let ``To string with title`` () =

        let xs = [2;3]
        let str = List.toStringWithTtl "abc" xs
        let f = str = "abc\n2\n3"

        Assert.True f