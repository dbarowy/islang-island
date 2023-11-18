module AST

type Point = {x: int; y: int}
type Circle = {x: Point; y: int}
type Dims = {x: int; y: int}
type Component =
| string
| Circle
| Point
type Definition = {name: string; components: Component list}
type Grammar = Definition list

let CANVAS_SZ = 400