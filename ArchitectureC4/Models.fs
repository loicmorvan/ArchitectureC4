module Models

type Component = {
    Identity: string
    Title: string
    Technology: string
    Description: string
}

type Container = {
    Identity: string
    Title: string
    Technology: string
    Description: string
    Components: Component list
}

type System = {
    Identity: string
    Title: string
    Description: string
    Containers: Container list
}

type Workspace = { Systems: System list }
