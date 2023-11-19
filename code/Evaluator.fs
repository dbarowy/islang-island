module Evaluator 
open AST

let evalName(name,scale) = ""
let evalCircle (point,radius, scale: Dims):string =
    let mininimum = min scale.w scale.h
    let width = scale.w/2
    let height = scale.h/2
    "   <circle cx =\"" +  (((point.x)+width+height) |> string) + "\"" +
    " cy =\"" +  ((point.y+width+height) |> string) + "\"" +
    " r =\"" + ((radius*(mininimum/5)) |> string) + "\"" +
    " stroke= \"black\" stroke-width=\"4\" fill =\"" + canvas_color + "\"/>\n"

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
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\" style=\"background-color:" + canvas_color+"\">" + "\n" +
    (evalCanvas canvas)
    + "</svg>\n"