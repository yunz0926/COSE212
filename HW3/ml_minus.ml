type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of (var * var * exp) * (var * var * exp) * env
and env = (var * value) list

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

exception UndefinedSemantics

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl
  

let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR v -> lookup_env v env
  | ADD(e1, e2) -> (match (eval e1 env, eval e2 env) with | (Int n1, Int n2) -> Int (n1 + n2) | _ -> raise UndefinedSemantics)
  | SUB(e1, e2) -> (match (eval e1 env, eval e2 env) with | (Int n1, Int n2) -> Int (n1 - n2) | _ -> raise UndefinedSemantics)
  | MUL(e1, e2) -> (match (eval e1 env, eval e2 env) with | (Int n1, Int n2) -> Int (n1 * n2) | _ -> raise UndefinedSemantics)
  | DIV(e1, e2) -> (match (eval e1 env, eval e2 env) with | (Int n1, Int n2) -> Int (n1 / n2) | _ -> raise UndefinedSemantics)
  | EQUAL(e1, e2) -> (match (eval e1 env, eval e2 env) with | (Int n1, Int n2) -> if (n1 == n2) then Bool true else Bool false 
                                                            | (Bool b1, Bool b2) -> if (b1 == b2) then Bool true else Bool false
                                                            | _ -> raise UndefinedSemantics)
  | LESS(e1, e2) -> (match (eval e1 env, eval e2 env) with | (Int n1, Int n2) -> if (n1 < n2) then Bool true else Bool false
                                                           | _ -> raise UndefinedSemantics)
  | NOT(e) -> (match eval e env with | Bool true -> Bool false | Bool false -> Bool true | _ -> raise UndefinedSemantics)
  | NIL -> List []
  | CONS(e1, e2) -> (match (eval e1 env, eval e2 env) with | (value, List l) -> List(value::l) |_ -> raise UndefinedSemantics)
  | APPEND(e1, e2) -> (match (eval e1 env, eval e2 env) with | (List l1, List l2) -> List(l1@l2) | _ -> raise UndefinedSemantics)
  | HEAD(e) -> (match eval e env with | List(v::s) -> v | _ -> raise UndefinedSemantics)
  | TAIL(e) -> (match eval e env with | List(v::s) -> List s | _ -> raise UndefinedSemantics)
  | ISNIL(e) -> (match eval e env with | List [] -> Bool true |_ -> Bool false)
  | IF(e1, e2, e3) -> (match eval e1 env with | Bool true -> eval e2 env | Bool false -> eval e3 env | _ -> raise UndefinedSemantics)
  | LET(x, e1, e2) -> let v1 = eval e1 env in eval e2 (extend_env (x, v1) env)
  | LETREC(v1, v2, e1, e2) -> let p = RecProcedure (v1, v2, e1, env) in eval e2 (extend_env (v1, p) env)
  | LETMREC((f1, v1, e1), (f2, v2, e2), e3) -> let p = MRecProcedure ((f1, v1, e1), (f2, v2, e2), env) in
                                               eval e3 (extend_env (f1, p) (extend_env (f2, p) env))
  | PROC(v, e) -> Procedure(v, e, env)
  | CALL(e1, e2) -> let p = eval e1 env in
                    let v = eval e2 env in
                    let extendedenv = (match p with 
                                       | Procedure(x, _, env1) -> extend_env  (x, v) env1
                                       | RecProcedure(f, x, _, env1) -> extend_env (f, p) (extend_env (x, v) env1)  
                                       | MRecProcedure ((f1, x, _), (f2, y, _), env1) -> (match e1 with | VAR funname -> 
                                            if((compare f1 funname) == 0) then extend_env (f2, p)(extend_env(f1, p)(extend_env (x, v) env1))
                                            else extend_env (f2, p)(extend_env(f1,p)(extend_env (y, v) env1))
                                       | _ -> raise UndefinedSemantics)) in 
                    let express = (match p with 
                                       | Procedure(_, e, _) -> e
                                       | RecProcedure(_, _, e, _) -> e
                                       | MRecProcedure ((f1, _, exp1), (f2, _, exp2), _) -> (match e1 with | VAR funname -> 
                                            if((compare f1 funname) == 0) then exp1 else exp2
                                       | _ -> raise UndefinedSemantics))
                    in eval express extendedenv

  | SEQ(e1, e2) -> let n = eval e1 env in eval e2 env;;
    
let runml : program -> value
=fun pgm -> eval pgm empty_env;;
