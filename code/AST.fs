module AST

type Point = {x: int; y: int}

type Dims = {w: int; h: int}

type Direction =
| Top
| Right
| Bottom
| Left
| TopRight
| TopLeft
| BottomRight
| BottomLeft

type Position = Position of Direction * int

type Rotation = Rotation of int

type Placement = Placement of Position * Rotation

type Component =
| Name of string
| Circle of Point * int
| Island of Placement
| Mountain of Placement
| Castle of Placement
| Cloud of Placement

type Definition = {name: string; dims: Dims; components: Component list}

type Canvas = Canvas of Definition list

let CANVAS_SZ = 400
let canvas_color = "navajowhite"
