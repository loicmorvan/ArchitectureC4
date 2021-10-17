module InterpreterTests

open Xunit
open Tokenizer
open Interpreter

let data = [
    [|
        [
            Word "workspace"
            Operator "{"
            Word "system"
            Operator "{"
            Operator "}"
            Operator "}"
        ] :> obj
        Interpreted.Workspace { Systems = [{ 
            Identity = "system1"
            Title = ""
            Description = ""
            Containers = []
            }] } :> obj
    |]
    [|
        [
            Word "workspace"
            Operator "{"
            Word "system"
            Operator "{"
            Operator "}"
            Word "system"
            Operator "{"
            Operator "}"
            Operator "}"
        ] :> obj
        Interpreted.Workspace { Systems = [
            { Identity = "system2"; Title = ""; Description = ""; Containers = [] }
            { Identity = "system1"; Title = ""; Description = ""; Containers = [] }
        ] } :> obj
    |]
    [|
        [
            Word "workspace"
            Operator "{"
            Word "system"
            Operator "{"
            Word "container"
            Operator "{"
            Operator "}"
            Word "container"
            Operator "{"
            Operator "}"
            Operator "}"
            Word "system"
            Operator "{"
            Word "container"
            Operator "{"
            Operator "}"
            Operator "}"
            Operator "}"
        ] :> obj
        Interpreted.Workspace { Systems = [
            { Identity = "system2"; Title = ""; Description = ""; Containers = [
                { Identity = "container1"; Title = ""; Technology = ""; Description = ""; Components = [] }
            ] }
            { Identity = "system1"; Title = ""; Description = ""; Containers = [
                { Identity = "container2"; Title = ""; Technology = ""; Description = ""; Components = [] }
                { Identity = "container1"; Title = ""; Technology = ""; Description = ""; Components = [] }
            ] }
        ] } :> obj
    |]
    [|
        [
            Word "workspace"
            Operator "{"
            Word "system"
            Text "The system"
            Operator "{"
            Operator "}"
            Operator "}"
        ] :> obj
        Interpreted.Workspace { Systems = [{ Identity = "system1"; Title = "The system"; Description = ""; Containers = [] }] } :> obj
    |]
    [|
        [
            Word "workspace"
            Operator "{"
            Word "system"
            Text "The system"
            Text "The description"
            Operator "{"
            Operator "}"
            Operator "}"
        ] :> obj
        Interpreted.Workspace { Systems = [{ Identity = "system1"; Title = "The system"; Description = "The description"; Containers = [] }] } :> obj
    |]
    [|
        [
            Word "workspace"
            Operator "{"
            Word "system32"
            Operator "="
            Word "system"
            Text "The system"
            Text "The description"
            Operator "{"
            Operator "}"
            Operator "}"
        ] :> obj
        Interpreted.Workspace { Systems = [{ Identity = "system32"; Title = "The system"; Description = "The description"; Containers = [] }] } :> obj
    |]
    [|
        [
            Word "workspace"
            Operator "{"
            Word "system32"
            Operator "="
            Word "system"
            Text "The system"
            Text "The description"
            Operator "{"
            Word "container16"
            Operator "="
            Word "container"
            Text "The container's title"
            Text "The container's technology"
            Text "The container's description"
            Operator "{"
            Operator "}"
            Operator "}"
            Operator "}"
        ] :> obj
        Interpreted.Workspace { 
            Systems = [
                {
                    Identity = "system32"
                    Title = "The system"
                    Description = "The description"
                    Containers = [
                        {
                            Identity = "container16"
                            Title = "The container's title"
                            Technology = "The container's technology"
                            Description = "The container's description"
                            Components = []
                        }
                    ]
                }
            ]
        } :> obj
    |]
]

[<Theory>]
[<MemberData(nameof(data))>]
let ``Test interpreter`` (tokens, expected) =
    let result = interpret tokens
    Assert.Equal(expected, result)
