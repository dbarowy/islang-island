module Evaluator 
open AST

let evalName(name,scale) = ""
let evalCircle (point,radius, scale: Dims):string = 
    "   <circle cx =\"" +  (((point.x)*scale.w*scale.h) |> string) + "\"" +
    "cy =\"" +  ((point.y*scale.w*scale.h) |> string) + "\"" +
    "r =\"" + ((radius*scale.w*scale.h) |> string) + "\"" +
    "stroke= \"green\" stroke-width=\"4\" fill =\"white\"/>\n"

let evalComponent (shape: Component, scale: Dims): string =
    match (shape, scale) with
    | Name (name),_ -> evalName (name,scale)
    | Circle (point,radius),_-> evalCircle(point,radius, scale)

let rec evalDefinition (def: Definition): string =
    match def.name, def.dims, def.components with
    | _,_,[] -> ""
    | n,x,a::ax -> evalComponent(a,x) + evalDefinition({name= n; dims = x; components = ax})

let rec evalCanvas(canvas: Canvas): string = 
    match canvas with
    | Canvas [] -> ""
    //| [x] -> evalDefinition (x)
    | Canvas (x::xs)-> evalDefinition (x) + evalCanvas(Canvas xs)

let rec eval (canvas: Canvas): string  = 
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    (evalCanvas canvas)
    + "</svg>\n"
