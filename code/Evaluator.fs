module Evaluator
open AST
open EvalLandscape

type Env = Map<string, Dims * string>

// let scalingFunction(outer_scale: Dims, inner_scale: Dims): ? =

let rec evalComponents
    (components: Component list)(scale: Dims)
    (env: Map<string, Dims * string>)
    : string =
    match components with
    | [] -> ""
    | x::xs -> 
        let eval1 = match x with
                    | Name (name) -> EvalLandscape.evalName (name, scale) env 
                    | Circle (point, radius) -> EvalLandscape.evalCircle(point, radius, scale)
                    // temp hack for now. to be fixed when evaluator is implemented
                    | _ -> ""
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
