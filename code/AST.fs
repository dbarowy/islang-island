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

type Position = Direction * int

type Rotation = int

type Placement = Position * Rotation

type Component =
| Name of string
| Circle of Point * int * Placement

type Definition = {name: string; dims: Dims; components: Component list}

type Canvas = Canvas of Definition list

let CANVAS_SZ = 400
let canvas_color = "navajowhite"