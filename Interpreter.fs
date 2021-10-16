module Interpreter

open Tokenizer

type Container = {
    Identity: string
    Title: string
    Technology: string
    Description: string
}
type System = {
    Identity: string
    Title: string
    Description: string
    Containers: Container list
}
type Workspace = { Systems: System list }

type State =
    | Start
    | WorkspaceReceived of Workspace
    | WorkspaceScope of Workspace
    | SystemIdentityReceived of Workspace * string
    | SystemAssignationReceived of Workspace * string
    | SystemReceived of Workspace * System
    | SystemTitleReceived of Workspace * System
    | SystemDescriptionReceived of Workspace * System
    | SystemScope of Workspace * System
    | ContainerReceived of Workspace * System * Container
    | ContainerScope of Workspace * System * Container
    | End of Workspace
    | Error of string

let findSystemId (systems: System list) =
    let mutable index = 1
    let mutable id = "system1"
    while systems |> List.exists (fun x -> x.Identity = id) do
        index <- index + 1
        id <- $"system{index}"
    id

let findContainerId (containers: Container list) =
    let mutable index = 1
    let mutable id = "container1"
    while containers |> List.exists (fun x -> x.Identity = id) do
        index <- index + 1
        id <- $"container{index}"
    id

let next state token =
    match state with
    | Start ->
        match token with
        | Word word when word = "workspace" -> WorkspaceReceived { Systems = [] }
        | _ -> Error $"Unexpected token {token}"
    | WorkspaceReceived workspace ->
        match token with
        | Operator operator when operator = "{" -> WorkspaceScope workspace
        | _ -> Error $"Unexpected token {token}"
    | WorkspaceScope workspace ->
        match token with
        | Operator operator when operator = "}" -> End workspace
        | Word keyword when keyword = "system" -> 
            let system = {
                Identity = findSystemId workspace.Systems
                Title = ""
                Description = ""
                Containers = []
            }
            SystemReceived (workspace, system)
        | Word identity -> SystemIdentityReceived (workspace, identity)
        | _ -> Error $"Unexpected token {token}"
    | SystemIdentityReceived (workspace, identity) ->
        match token with
        | Operator equality when equality = "=" -> SystemAssignationReceived (workspace, identity)
        | _ -> Error $"Unexpected token {token}"
    | SystemAssignationReceived (workspace, identity) ->
        match token with
        | Word keyword when keyword = "system" -> 
            let system = {
                Identity = identity
                Title = ""
                Description = ""
                Containers = []
            }
            SystemReceived (workspace, system)
        | _ -> Error $"Unexpected token {token}"
    | SystemReceived (workspace, system) ->
        match token with
        | Text title ->
            let newSystem = { system with Title = title }
            SystemTitleReceived (workspace, newSystem)
        | Operator operator when operator = "{" -> SystemScope (workspace, system)
        | _ -> Error $"Unexpected token {token}"
    | SystemTitleReceived (workspace, system) ->
        match token with
        | Text description ->
            let newSystem = { system with Description = description }
            SystemDescriptionReceived (workspace, newSystem)
        | Operator operator when operator = "{" -> SystemScope (workspace, system)
        | _ -> Error $"Unexpected token {token}"
    | SystemDescriptionReceived (workspace, system) ->
        match token with
        | Operator operator when operator = "{" -> SystemScope (workspace, system)
        | _ -> Error $"Unexpected token {token}"
    | SystemScope (workspace, system) ->
        match token with
        | Operator operator when operator = "}" ->
            let newWorkspace = { workspace with Systems = system :: workspace.Systems }
            WorkspaceScope newWorkspace
        | Word keyword when keyword = "container" ->
            let container = {
                Identity = findContainerId system.Containers
                Title = ""
                Technology = ""
                Description = ""
            }
            ContainerReceived (workspace, system, container)
        | _ -> Error $"Unexpected token {token}"
    | ContainerReceived (workspace, system, container) ->
        match token with
        | Operator operator when operator = "{" -> ContainerScope (workspace, system, container)
        | _ -> Error $"Unexpected token {token}"
    | ContainerScope (workspace, system, container) ->
        match token with
        | Operator operator when operator = "}" ->
            let newSystem = { system with Containers = container :: system.Containers }
            SystemScope (workspace, newSystem)
        | _ -> Error $"Unexpected token {token}"
    | End workspace -> End workspace
    | Error error -> Error error

type Interpreted =
    | Workspace of Workspace
    | Error of string

let interpret tokens =
    let mutable state = Start
    for token in tokens do
        state <- next state token
    match state with
    | End workspace -> Workspace workspace
    | State.Error error -> Error error
    | _ -> Error "error"
