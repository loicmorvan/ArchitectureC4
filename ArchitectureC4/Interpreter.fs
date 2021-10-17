module Interpreter

open Tokenizer
open Models

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
    | ComponentIdentityReceived of Workspace * System * Container * string
    | ComponentAssignationReceived of Workspace * System * Container * string
    | ComponentKeywordReceived of Workspace * System * Container * Component
    | ComponentTitleReceived of Workspace * System * Container * Component
    | ComponentTechnologyReceived of Workspace * System * Container * Component
    | ComponentDescriptionReceived of Workspace * System * Container * Component
    | ComponentScope of Workspace * System * Container * Component
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
                Components = []
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
                Components = []
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
        | Word keyword when keyword = "component" ->
            let comp = {
                Identity = ""
                Title = ""
                Technology = ""
                Description = ""
            }
            ComponentKeywordReceived (workspace, system, container, comp)
        | Word identity -> ComponentIdentityReceived (workspace, system, container, identity)
        | _ -> Error $"Unexpected token {token}"
    | ComponentIdentityReceived (workspace, system, container, identity) ->
        match token with
        | Operator equality when equality = "=" -> ComponentAssignationReceived (workspace, system, container, identity)
        | _ -> Error $"Unexpected token {token}"
    | ComponentAssignationReceived (workspace, system, container, identity) ->
        match token with
        | Word keyword when keyword = "component" ->
            let comp = {
                Identity = identity
                Title = ""
                Technology = ""
                Description = ""
            }
            ComponentKeywordReceived (workspace, system, container, comp)
        | _ -> Error $"Unexpected token {token}"
    | ComponentKeywordReceived (workspace, system, container, comp) ->
        match token with
        | Text title -> ComponentTitleReceived (workspace, system, container, { comp with Title = title })
        | _ -> Error $"Unexpected token {token}"
    | ComponentTitleReceived (workspace, system, container, comp) ->
        match token with
        | Text technology -> ComponentTechnologyReceived (workspace, system, container, { comp with Technology = technology })
        | _ -> Error $"Unexpected token {token}"
    | ComponentTechnologyReceived (workspace, system, container, comp) ->
        match token with
        | Text description -> ComponentDescriptionReceived (workspace, system, container, { comp with Description = description })
        | _ -> Error $"Unexpected token {token}"
    | ComponentDescriptionReceived (workspace, system, container, comp) ->
        match token with
        | Operator "{" -> ComponentScope (workspace, system, container, comp)
        | _ -> Error $"Unexpected token {token}"
    | ComponentScope (workspace, system, container, comp) ->
        match token with
        | Operator "}" -> ContainerScope (workspace, system, { container with Components = comp :: container.Components })
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
