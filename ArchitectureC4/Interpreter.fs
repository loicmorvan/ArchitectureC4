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
    | SystemKeywordReceived of Workspace * System
    | SystemTitleReceived of Workspace * System
    | SystemDescriptionReceived of Workspace * System
    | SystemScope of Workspace * System
    | ContainerIdentityReceived of Workspace * System * string
    | ContainerAssignationReceived of Workspace * System * string
    | ContainerKeywordReceived of Workspace * System * Container
    | ContainerTitleReceived of Workspace * System * Container
    | ContainerTechnologyReceived of Workspace * System * Container
    | ContainerDescriptionReceived of Workspace * System * Container
    | ContainerScope of Workspace * System * Container
    | End of Workspace
    | Error of string
    
let findId (list: 'a list) (baseName: string) (predicate: string -> 'a -> bool) =
    let mutable index = 1
    let mutable id = $"{baseName}{index}"
    while list |> List.exists (predicate id) do
        index <- index + 1
        id <- $"{baseName}{index}"
    id
    
let findSystemId (systems: System list) = findId systems "system" (fun name system -> system.Identity = name)
let findContainerId (containers: Container list) = findId containers "container" (fun name container -> container.Identity = name)

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
            SystemKeywordReceived (workspace, system)
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
            SystemKeywordReceived (workspace, system)
        | _ -> Error $"Unexpected token {token}"
    | SystemKeywordReceived (workspace, system) ->
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
            ContainerKeywordReceived (workspace, system, container)
        | Word identity -> ContainerIdentityReceived (workspace, system, identity)
        | _ -> Error $"Unexpected token {token}"
    | ContainerIdentityReceived (workspace, system, identity) ->
        match token with 
        | Operator equality when equality = "=" -> ContainerAssignationReceived (workspace, system, identity)
        | _ -> Error $"Unexpected token {token}"
    | ContainerAssignationReceived (workspace, system, identity) ->
        match token with 
        | Word keyword when keyword = "container" -> 
            let container = {
                Identity = identity
                Title = ""
                Technology = ""
                Description = ""
            }
            ContainerKeywordReceived (workspace, system, container)
        | _ -> Error $"Unexpected token {token}"
    | ContainerKeywordReceived (workspace, system, container) ->
        match token with
        | Text title -> ContainerTitleReceived (workspace, system, { container with Title = title })
        | Operator operator when operator = "{" -> ContainerScope (workspace, system, container)
        | _ -> Error $"Unexpected token {token}"
    | ContainerTitleReceived (workspace, system, container) ->
        match token with
        | Text technology -> ContainerTechnologyReceived (workspace, system, { container with Technology = technology })
        | Operator operator when operator = "{" -> ContainerScope (workspace, system, container)
        | _ -> Error $"Unexpected token {token}"
    | ContainerTechnologyReceived (workspace, system, container) ->
        match token with
        | Text description -> ContainerTechnologyReceived (workspace, system, { container with Description = description })
        | Operator operator when operator = "{" -> ContainerScope (workspace, system, container)
        | _ -> Error $"Unexpected token {token}"
    | ContainerDescriptionReceived (workspace, system, container) ->
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
