module AST

type Point = {x: int; y: int}
type Circle = {x: Point; y: int}
type Dims = {x: int; y: int}
type Component =
| string
| Circle of Circle
| Point of Point

type Name = string
type Definition = {name: Name; dims: Dims; components: Component list}
type Canvas = Canvas of Definition list * string

let CANVAS_SZ = 400