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



let relativeToAbsPos(dims: Dims)(pos: Position): Point =
    let direction,units = match pos with Position (x,y) -> x,y
    let diag_units = int (0.707 * float units)

    let scaled_unit_x = int (dims.w / 3)
    let scaled_unit_y = int (dims.h / 3)

    match direction with
    | Top -> {x=0; y=(- scaled_unit_y * units)}
    | Bottom -> {x=0; y=(scaled_unit_y * units)}
    | Left -> {x=(- scaled_unit_x*units); y=0}
    | Right -> {x=(scaled_unit_x*units); y=0}
    | TopLeft ->{x=(-scaled_unit_x*diag_units); y=(-scaled_unit_y*diag_units)} 
    | TopRight ->{x=(scaled_unit_x*diag_units); y=(-scaled_unit_y*diag_units)} 
    | BottomRight ->{x=(scaled_unit_x*diag_units); y=(scaled_unit_y*diag_units)} 
    | BottomLeft ->{x=(-scaled_unit_x*diag_units); y=(scaled_unit_y*diag_units)} 



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
