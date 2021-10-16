module TokenizerTests

open Xunit
open Tokenizer

let data = [
    [|
        "hello { world }" :> obj;
        [
            Word "hello"
            Operator "{"
            Word "world"
            Operator "}"
        ] :> obj
    |]
    [|
        "hello { \"world !\" }" :> obj;
        [
            Word "hello"
            Operator "{"
            Text "world !"
            Operator "}"
        ] :> obj
    |]
]

[<Theory>]
[<MemberData(nameof(data))>]
let ``Tokenize an input`` (input, expected) =
    let tokens = tokenize input
    Assert.Equal<Token list>(expected, tokens)
