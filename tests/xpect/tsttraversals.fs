module Testlee.DSystems.Traversals

open Expecto

open Simplee.DSystems

[<Tests>]
let tests = 
    testList "traversals" [
        testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
            let subject = true
            Expect.isTrue subject "I compute, therefore I am."
    ]
