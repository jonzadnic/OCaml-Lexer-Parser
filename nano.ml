exception MLFailure of string

type binop =
  Plus
| Minus
| Mul
| Div
| Eq
| Ne
| Lt
| Le
| And
| Or
| Cons

type expr =
  Const of int
| True
| False
| NilExpr
| Var of string
| Bin of expr * binop * expr
| If  of expr * expr * expr
| Let of string * expr * expr
| App of expr * expr
| Fun of string * expr
| Letrec of string * expr * expr

type value =
  Int of int
| Bool of bool
| Closure of env * string option * string * expr
| Nil
| Pair of value * value

and env = (string * value) list

let binopToString op =
  match op with
      Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v =
  match v with
    Int i ->
      Printf.sprintf "%d" i
  | Bool b ->
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) ->
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) ->
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2)
  | Nil ->
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | NilExpr -> "[]"
    | True ->
        "true"
    | False ->
        "false"
    | Var x ->
        x
    | Bin (e1,op,e2) ->
        Printf.sprintf "%s %s %s"
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) ->
        Printf.sprintf "if %s then %s else %s"
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) ->
        Printf.sprintf "let %s = %s in \n %s"
        x (exprToString e1) (exprToString e2)
    | App (e1,e2) ->
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) ->
        Printf.sprintf "fun %s -> %s" x (exprToString e)
    | Letrec (x,e1,e2) ->
        Printf.sprintf "let rec %s = %s in \n %s"
        x (exprToString e1) (exprToString e2)

(*********************** Some helpers you might need ***********************)

let rec fold f base args =
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) =
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(*This function literally looks up the first value in a list of key-value pairs given the key.*)
let lookup (x,evn) =
  let a = listAssoc (x, evn) in
  match a with
  | Some value -> value
  | None -> raise  ( MLFailure ("Variable not bound: " ^ x ))

let rec eval (evn,e) =
  match e with
  | Const i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var s -> lookup (s,evn)
  | Bin (expr1, op, expr2) ->
  (*Every expression can be reduced to two expressions and an operator in between*)
    let val1 = eval (evn, expr1) in
    let val2 = eval (evn, expr2) in
    (*val1 and val2 respectively evaluate the two expressions based on the current environment.*)
    ( match (val1, op, val2) with
    | (Int i1, Plus , Int i2 ) -> Int ( i1 + i2 )
    | (Int i1, Minus, Int i2 )  -> Int ( i1 - i2)
    | (Int i1, Mul, Int i2 ) -> Int (i1 * i2)
    | (Int i1, Div, Int i2 ) ->  Int (i1 / i2)
    | (Bool b1, Eq, Bool b2) -> Bool ( b1 = b2)
    | (Int i1, Eq, Int i2) -> Bool ( i1 = i2)
    | (Bool b1, Ne, Bool b2) -> Bool (b1 != b2)
    | (Int i1, Ne, Int i2) -> Bool (i1 != i2)
    | (Int i1, Lt, Int i2) -> Bool (i1 < i2)
    | (Int i1, Le, Int i2) -> Bool (i1 <= i2)
    | (Bool b1, And, Bool b2) -> Bool (b1 && b2)
    | (Bool b1, Or, Bool b2) -> Bool (b1 || b2)
    | (Int expr1 ,Cons, Nil) -> Pair (Int expr1, Nil)
    | (Int expr1, Cons, Pair(x,y)) -> Pair (Int expr1, (eval (evn, expr2)))
    | _ -> raise (MLFailure ("Invalid statement") )
    )

  | If (e1,e2,e3) -> ((*First evaluate the condition. If true, then evaluate e2, else e3*)
                       match eval (evn , e1) with
                        |  Bool true -> eval (evn, e2)
                        |  Bool false -> eval (evn, e3)
                        |  _ -> raise (MLFailure ("Invalid statement") )
                      )

  (*First evaluate the first expression, then map the
  name of the function to this value and store the
  pair in the environment, then evaluate the second expression.*)
  | Let (name, e1, e2) ->
    let x1 = eval (evn, e1) in
    let evn_nuevo = (name,x1)::evn in
    eval(evn_nuevo, e2)

  (*let rec function should be stored in a Closure
  First evaluate expression 1, if it is a Closure, then recursively
  evaluate this Closure. In the other case, the function can be evaluated directly.*)
  | Letrec (name, e1, e2) ->
    let v1 = eval (evn, e1) in
    (match v1 with
     | Closure (evn1,_,str,e) -> eval (((name,(Closure(evn1, Some name, str, e)))::evn),e2)
     | _ -> eval ((name, v1)::evn, e2)
    )

  | App (e1,e2) ->
        (match e1 with
        | Var "hd" -> (match eval(evn, e2) with Pair (e1, e2) -> e1)
        | Var "tl" -> (match eval(evn, e2) with Pair (e1, e2) -> e2)
        | _ ->
              (let Closure (fun_evn, f, x, e) = eval (evn, e1) in
               let new_evn = match f with
                 | Some i -> (i, Closure (fun_evn, f, x, e))::(x, eval(evn, e2))::fun_evn
                 | None -> (x, eval(evn, e2))::fun_evn
               in
                 eval (new_evn, e)))
  | Fun (s, e1) -> Closure (evn, None, s , e1) (*A Function should be evaluated as a Closure.*)
  | NilExpr -> Nil
  | _ -> raise (MLFailure ("Invalid statement") )


(**********************     Testing Code  ******************************)
