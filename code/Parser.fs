module Parser

open Combinator
open AST

(*
    TODO (Ammar):
    1) Add comments to functions in parser
    2) Fig bug in example-3: Castle is being interpreted as a definition (name)
    instead of a primitive
    3) Film video presi
*)

let pdefn,pdefnImpl = recparser()

(* my_ws
 *   Consider any non-newline whitespace to be whitespace
 *)
let my_ws = (pwsNoNL0 |>> (fun _ -> true))

(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween my_ws p my_ws

(* pnum
 *   Parses a number.
 *)
let pnum: Parser<int> = pmany1 pdigit
                         |>> (fun ds ->
                                int (stringify ds)
                             ) <!> "pnum"

let ptwonums: Parser<int*int> = pseq
                                    (pad pnum)
                                    (pright (pchar ',') (pad pnum))
                                    (fun (x,y) -> (x,y)) <!> "ptwonums"

let ppoint: Parser<Point> = pright 
                                (pstr "point=")
                                (pbetween (pchar '(') ptwonums (pchar ')'))
                                |>> (fun (xval,yval) -> {x=xval; y=yval})
                                <!> "ppoint"

let pradius: Parser<int> = pright (pstr "radius=") (pad pnum) <!> "pradius"

let pcircle: Parser<Component> = pright 
                                    (pstr "circle")
                                    (pseq
                                        (pad ppoint)
                                        (pad pradius)
                                        (fun (x,y) ->
                                            Circle(x,y))
                                    ) <!> "pcircle"

(* pname
 *   Parses a variable.  Variable names are at least one
 *   character long, starting with a letter, followed by
 *   any combination of letters or numbers.
 *)
let pnamechar: Parser<char> = pletter <|> pdigit <!> "pnamechar"

let pname: Parser<string> = pseq
                                pletter 
                                (pmany0 pnamechar |>> stringify)
                                (fun (c: char, s: string) -> (string c) + s) <!> "pname"

let pdirection: Parser<Direction> = (pad
                                        ((pstr "top-right") <|> (pstr "top-left") <|> (pstr "bottom-right") <|> (pstr "bottom-left")
                                        <|> (pstr "right") <|> (pstr "left") <|> (pstr "top") <|> (pstr "bottom"))
                                    ) |>> fun x ->
                                        match x with
                                        | "bottom-left" -> BottomLeft
                                        | "bottom-right" -> BottomRight
                                        | "top-right" -> TopRight
                                        | "top-left" -> TopLeft
                                        | "right" -> Right
                                        | "left" -> Left
                                        | "top" -> Top
                                        | "bottom" -> Bottom
                                        // default for error but won't happen
                                        | _ -> Top
                                                                

let pposition: Parser<Position> = pseq
                                    (pleft (pad pnum) (pstr "units to the"))
                                    pdirection
                                    (fun (u, dir) -> Position(dir, u)) <!> "pposition"

let protation: Parser<int> = pright 
                                (pad (pstr "rotated"))
                                pnum
                                <!> "protation"

let pisland: Parser<string> = pstr "island" <!> "pisland"
let pmountain: Parser<string> = pstr "mountain" <!> "pmountain"
let pcastle: Parser<string> = pstr "castle" <!> "pcastle"
let pcloud: Parser<string> = pstr "cloud" <!> "pcloud"

let prelative: Parser<Placement> =  pseq
                                        (pmany0 pposition)
                                        (pmany0 protation)
                                        (fun (pos_list, dir_list) -> 
                                            match (pos_list, dir_list) with
                                            | ([],[]) -> RelativePlacement(Position(Top, 0), 0)
                                            | (x::xs,[]) -> RelativePlacement(x, 0)
                                            | ([],y::ys) -> RelativePlacement(Position(Top, 0), y)
                                            | (x::xs, y::ys) -> RelativePlacement(x,y)
                                        ) <!> "prelative"

let pcompound: Parser<Component> = pseq
                                        (pad (pisland <|> pmountain <|> pcastle <|> pcloud <|> pname))
                                        prelative
                                        //pplacement
                                        (fun (name, placement) ->
                                            match name with
                                            | "ssland" -> Island(placement)
                                            | "mountain" -> Mountain(placement)
                                            | "castle" -> Castle(placement)
                                            | "cloud" -> Cloud(placement)
                                            // will never happen but to keep f# happy
                                            | _ -> Name(name, placement)
                                            ) <!> "pcompound"

let pcomponent: Parser<Component> = pright pws1 (pcircle <|> pcompound) <!> "pcomponent"

let pcomponents: Parser<Component list> = pmany1
                                            (pleft pcomponent pnl)
                                            <!> "pcomponents"

let pdims: Parser<Dims> = pseq
                            pnum
                            (pbetween (pchar 'x') pnum (pleft (pstr " is:") pnl))
                            (fun (x,y) -> {w=x; h=y})
                        <!> "pdims"

pdefnImpl := pseq
                (pseq pname (pright pws1 pdims) (fun (x,y) -> (x,y))) 
                (pcomponents) 
                (fun ((x,y),z) -> {name=x; dims=y; components=z})
                <!> "pdefn"

(* pdefns
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pdefns = pmany1 (pleft pdefn pws0 ) |>> Canvas <!> "pdefns"

(* grammar
 *  Top level parser definition.  Call this one
 *  if you want a Blub parser.
 *)
let canvas = pleft pdefns peof <!> "canvas"

(* parse
 *  User-friendly function that calls the Blub parser
 *  and returns an optional Expr.
 *)
let parse (input: string)(do_debug: bool) : Canvas option =
    let i = (if do_debug then debug else prepare) input
    match canvas i with
    | Success(ast,_) -> Some ast
    | Failure(_,_)   -> None
