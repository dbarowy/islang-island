module Evaluator
open AST
open EvalLandscape


type Env = Map<string, Dims * Component list>

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

let evalFirstDefinition(def: Definition)(env: Env): Definition * Env =
    match def.name, def.dims, def.components with
    | n, dim,_ -> 
        let absComponents = evalFirstComponents dim def.components
        let env1 = env.Add(n, (dim, absComponents))
        {name = n; dims = dim ; components = absComponents}, env1

let rec makeCoordinates(canvas: Canvas)(env: Env): Definition list * Env =
    match canvas with
        | Canvas [] -> [], env
        | Canvas (x::xs)-> 
            let def1, env1 = evalFirstDefinition x env
            let def_rest, env2 = makeCoordinates(Canvas xs) env1
            def1 :: def_rest, env2

//--------------------------------------------------------

let combineScales(outer_scale: Dims, inner_scale: Dims): Dims =
    inner_scale

let combinePositions(outer_pos: Point, inner_pos: Point): Point =
    {x=outer_pos.x+inner_pos.x; y=outer_pos.y+inner_pos.y}

let rtn_fields (place: Placement) : Point*Dims*int =
    match place with 
        | AbsPlacement(u, y, z) -> u,y,z
        | _ ->  {x= 0;y=0}, {w= 0;h=0}, 0

let rec evalComponents
    (components: Component list)(outer_placement: Placement)
    (env: Env): string =

    let outer_position,outer_scale, outer_rotation =
        rtn_fields(outer_placement)
    
    match components with
    | [] -> ""
    | x::xs -> 
        let eval1 = match x with
                    //TODO combine rotation
                    | Circle (point, radius) -> evalCircle(point, radius, outer_scale)

                    | Island (inner_placement)  -> 
                        let inner_position, inner_scale, inner_rotation = 
                            rtn_fields(inner_placement)
                        evalIsland(combinePositions(outer_position,inner_position), outer_scale)

                    | Mountain (inner_placement) ->
                        let inner_position, inner_scale, inner_rotation = 
                            rtn_fields(inner_placement)
                        evalMountain(combinePositions(outer_position,inner_position), outer_scale)

                    | Castle (inner_placement) ->
                        let inner_position, inner_scale, inner_rotation = 
                            rtn_fields(inner_placement)
                        evalCastle(combinePositions(outer_position,inner_position), outer_scale)

                    | Cloud (inner_placement) ->
                        let inner_position, inner_scale, inner_rotation = 
                            rtn_fields(inner_placement)
                        evalCloud(combinePositions(outer_position,inner_position), outer_scale)

                    | Name (name, inner_placement) -> 
                        if Map.containsKey name env then
                            let dims, ast = env[name]
                            let inner_position, inner_scale, inner_rotation = rtn_fields(inner_placement)
                            let combined_position = combinePositions(outer_position, inner_position)
                            let newplacement = AbsPlacement(combined_position, dims, inner_rotation)

                            evalComponents ast newplacement env

                            
                            else
                                printfn "Undefined variable."
                                exit 1

        let eval2 = evalComponents xs outer_placement env
        eval1 + eval2


let evalDefinition
    (def: Definition)(placement: Placement)(env: Env)
    : string * Env =

    match def.name, def.dims, def.components with
    | _,_,[] -> ("",env)
    | n,x,_ ->
        let svg_str = evalComponents def.components placement env
        svg_str, env


let eval (canvas: Canvas): string  = 
    let pass, env = makeCoordinates canvas Map.empty
    let last_def = List.head (List.rev pass)
    let csz = CANVAS_SZ |> string
    let abs_coordinates = AbsPlacement({x=0;y=0}, {w=400; h=400}, 0)
    let out_str, _ = evalDefinition last_def abs_coordinates env

    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\" style=\"background-color:" + canvas_color+"\">" + "\n" +
    (out_str)
    + "</svg>\n"

//----------------------------------------------------------
