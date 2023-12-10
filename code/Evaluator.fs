module Evaluator
open AST
open EvalLandscape

type Env = Map<string, Dims * string>

// let combineScales(outer_scale: Dims, inner_scale: Dims): ? =
// let combinePositions(outer_pos: Point, inner_pos: Point): ? =

let rec evalComponents
    (components: Component list)(scale: Dims)
    (env: Map<string, Dims * string>)
    : string =
    match components with
    | [] -> ""
    | x::xs -> 
        let eval1 = match x with
                    //TODO: Name
                    | Name (name, placement) -> evalName (name, scale) env 
                    | Circle (point, radius) -> evalCircle(point, radius, scale)
                    | Island (placement)  -> evalIsland(placement, scale)
                    | Mountain (placement) -> evalMountain(placement, scale)
                    | Castle (placement) -> evalCastle(placement, scale)
                    | Cloud (placement) -> evalCloud(placement, scale)
        let eval2 = evalComponents xs scale env
        eval1 + eval2


let evalDefinition
    (def: Definition)(env: Map<string, Dims * string>)
    : string * Map<string, Dims * string> =

    match def.name, def.dims, def.components with
    | _,_,[] -> ("",env)
    | n,x,_ ->
        let svg_str = evalComponents def.components x env
        let env1 = env.Add(n, (x , svg_str))
        svg_str, env1


let rec evalCanvas(canvas: Canvas)(env: Map<string, Dims * string>): string = 
    match canvas with
    | Canvas [] -> ""
    | Canvas [x] ->
        let eval, env1 = evalDefinition (x)(env)
        eval
    | Canvas (x::xs)->
        let _, env1 = evalDefinition (x) (env)
        evalCanvas(Canvas xs)(env1)


let eval (canvas: Canvas): string  = 
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\" style=\"background-color:" + canvas_color+"\">" + "\n" +
    (evalCanvas canvas Map.empty)
    + "</svg>\n"

//----------------------------------------------------------



// user can give in any of 8 directions in units between 1 and 3
let relativeToAbsPos(dims: Dims)(pos: Position): Point =

    let direction,units = match pos with Position (x,y) -> x, float y

    let scaled_unit_x = float dims.w / 6.0
    let scaled_unit_y = float dims.h / 6.0

    let tpoint = match direction with
                    | Top -> {x=0; y= int (- scaled_unit_y * units)}
                    | Bottom -> {x=0; y= int (scaled_unit_y * units)}
                    | Left -> {x= int (- scaled_unit_x*units); y=0}
                    | Right -> {x= int (scaled_unit_x*units); y=0}
                    | TopLeft ->{x= int (-scaled_unit_x*units); y= int (-scaled_unit_y*units)} 
                    | TopRight ->{x= int (scaled_unit_x*units); y= int (-scaled_unit_y*units)} 
                    | BottomRight ->{x= int (scaled_unit_x*units); y= int (scaled_unit_y*units)} 
                    | BottomLeft ->{x= int (-scaled_unit_x*units); y= int (scaled_unit_y*units)} 
    
    {x=int(3.0 * scaled_unit_x) + tpoint.x; y=int(3.0 * scaled_unit_y) + tpoint.y}



let relativeToAbs(dims: Dims)(placement: Placement): Placement =
    match placement with
    | RelativePlacement (pos, rotation) -> 
        AbsPlacement (relativeToAbsPos(dims)(pos), dims, rotation)
    | _ -> placement 

let rec evalFirstComponents(dims: Dims)(components: Component list): Component list =
    match components with
        | [] -> []
        | x::xs -> 
            let eval1 = match x with
                        | Name (name, placement) -> Name(name, relativeToAbs dims placement)
                        | Circle (point, radius) -> Circle(point, radius)
                        | Island (placement)  -> Island (relativeToAbs dims placement)
                        | Mountain (placement) -> Mountain (relativeToAbs dims placement)
                        | Castle (placement) -> Castle (relativeToAbs dims placement)
                        | Cloud (placement) -> Cloud (relativeToAbs dims placement)
            eval1 :: (evalFirstComponents dims xs)

let evalFirstDefinition(def: Definition): Definition =
    match def.name, def.dims, def.components with
    | n,x,_ -> {name = n; dims = x ; components = (evalFirstComponents x def.components)}

let rec makeCoordinates(canvas: Canvas): Definition list =
    match canvas with
        | Canvas [] -> []
        | Canvas (x::xs)-> evalFirstDefinition(x) :: makeCoordinates(Canvas xs)

let firstPass(canvas: Canvas): Canvas =
    Canvas (makeCoordinates canvas)
