type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp                 (* sequence *)
  | IF of exp * exp * exp            (* if-then-else *)
  | WHILE of exp * exp               (* while loop *)
  | LETV of id * exp * exp           (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list           (* call by value *)
  | CALLR of id * id list            (* call by referenece *)
  | RECORD of (id * exp) list        (* record construction *)
  | FIELD of exp * id                (* access record field *)
  | ASSIGN of id * exp               (* assgin to variable *)
  | ASSIGNF of exp * id * exp        (* assign to record field *)
  | WRITE of exp
and id = string

type loc = int
type value =
| Num of int
| Bool of bool
| Unit
| Record of record 
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

(********************************)
(*     Handling environment     *)
(********************************)

let rec lookup_loc_env : id -> env -> loc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind (id,l) -> if(x=id) then l else lookup_loc_env x tl
    | ProcBind _ -> lookup_loc_env x tl
    end

let rec lookup_proc_env : id -> env -> proc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind _ -> lookup_proc_env x tl
    | ProcBind (id,binding) -> if (x=id) then binding else lookup_proc_env x tl
    end
    
let rec lookup_env : id -> env -> binding
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind (id,l) -> if(x=id) then hd else lookup_env x tl
    | ProcBind (id, p) -> if(x=id) then hd else lookup_env x tl
    end
  

let extend_env : binding -> env -> env
= fun e env -> e::env

let empty_env = []


(***************************)
(*     Handling memory     *)
(***************************)

let rec lookup_mem : loc -> memory -> value
= fun l mem ->
  match mem with
  | [] -> raise(Failure ("location "^(string_of_int l)^" is not included in memory"))
  | (loc,v)::tl -> if(l=loc) then v else lookup_mem l tl

let extend_mem : (loc * value) -> memory -> memory
= fun (l,v) mem -> (l,v)::mem

let empty_mem = []

(***************************)
(*     Handling record     *)
(***************************)

let rec lookup_record : id -> record -> loc
= fun id record -> 
  match record with
    | [] -> raise(Failure ("field "^ id ^" is not included in record"))
    | (x,l)::tl -> if (id=x) then l else lookup_record id tl


let extend_record : (id * loc) -> record -> record
= fun (x,l) record -> (x,l)::record

let empty_record = []

(***************************)

let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

let rec list_fold2 : ('a -> 'b -> 'c -> 'c)-> 'a list -> 'b list -> 'c -> 'c
= fun func l1 l2 acc ->
  match (l1,l2) with
  | ([],[]) -> acc
  | (hd1::tl1,hd2::tl2) -> list_fold2 func tl1 tl2 (func hd1 hd2 acc)
  | _ -> raise (Failure "two lists have different length")

let rec list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun func l acc ->
  match l with
  | [] -> acc
  | hd::tl -> list_fold func tl (func hd acc)

let value2str : value -> string
= fun v ->
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "unit"
  | Record _ -> "record" 
  
let rec allocatemem : id list ->loc list -> loc list
= fun id loc ->
  match id with
  | [] -> loc
  | (hd::tl) -> allocatemem tl loc@[new_location()]

let rec extend_idlist_loclist : env -> id list -> loc list -> env
= fun env id loc ->
  match (id, loc) with
  | ([], []) -> env
  | (h1::t1, h2::t2) -> extend_idlist_loclist (extend_env (LocBind(h1, h2))env) t1 t2
  | _ -> raise UndefinedSemantics
  
let rec extend_idlist_bindlist: env -> id list -> binding list -> env
= fun env id bind ->
  match (id, bind) with
  | ([], []) -> env
  | (h1::t1, h2::t2) -> let ref = match h2 with | LocBind(_, l) -> LocBind(h1, l) | ProcBind(_, p) -> ProcBind(h1, p)
    in extend_idlist_bindlist (extend_env ref env) t1 t2
  | _ -> raise UndefinedSemantics
  
let rec get_bindlist : env -> id list -> binding list -> binding list
= fun env id bind ->
  match id with
  | [] -> bind
  | (hd::tl) -> get_bindlist env tl bind@[lookup_env hd env] 
                
  
let rec extend_loclist_valuelist : memory -> loc list -> value list -> memory
= fun mem loc value ->
  match (loc, value) with
  | ([], []) -> mem
  | (h1::t1, h2::t2) -> extend_loclist_valuelist (extend_mem (h1, h2) mem) t1 t2
  | _ -> raise UndefinedSemantics
  
let rec separate_tuple : 'a list -> 'b list * 'c list -> 'b list * 'c list
= fun l separate -> let (blist, clist) = separate in
  match l with
  | [] -> separate
  | (hd::tl) -> (match hd with (b, c) -> separate_tuple tl (blist@[b], clist@[c]))
  
let rec combine_tuple : 'a list -> 'b list -> 'c list -> 'c list
= fun l1 l2 combinelist ->
  match (l1, l2) with
  | ([], []) -> combinelist
  | (h1::t1, h2::t2) -> combine_tuple t1 t2 combinelist@[(h1, h2)]
  | _ -> raise UndefinedSemantics
  
  

let rec eval_aop : env -> memory -> exp -> exp -> (int -> int -> int) -> (value * memory)
= fun env mem e1 e2 op ->
  let (v1,mem1) = eval env mem e1 in
  let (v2,mem2) = eval env mem1 e2 in
  match (v1,v2) with
  | (Num n1, Num n2) -> (Num (op n1 n2), mem2)
  | _ -> raise (Failure "arithmetic operation type error")

and eval : env -> memory -> exp -> (value * memory) 
=fun env mem e ->
  let rec eval_explist : env -> exp list -> value list * memory -> value list * memory
  = (fun env explist valuelist_mem  -> let (valuelist, mem) = valuelist_mem in
    match explist with
    | [] -> valuelist_mem
    | (hd::tl) -> let (v0, mem0) = (eval env mem hd) in (eval_explist env tl (valuelist@[v0], mem0))) in
  match e with
  | WRITE e -> 
    let (v1,mem1) = eval env mem e in
    let _ = print_endline(value2str v1) in
    (v1,mem1)
  | NUM n -> (Num n, mem) 
  | TRUE -> (Bool true, mem) 
  | FALSE -> (Bool false, mem) 
  | UNIT -> (Unit, mem)
  | VAR v -> (lookup_mem (lookup_loc_env v env) mem, mem)
  | ADD (e1, e2) -> eval_aop env mem e1 e2 (fun n1 n2 -> n1 + n2)
  | SUB (e1, e2) -> eval_aop env mem e1 e2 (fun n1 n2 -> n1 - n2)
  | MUL (e1, e2) -> eval_aop env mem e1 e2 (fun n1 n2 -> n1 * n2)
  | DIV (e1, e2) -> eval_aop env mem e1 e2 (fun n1 n2 -> n1 / n2)
  | EQUAL (e1, e2) -> (let (v1, mem1) = eval env mem e1 in
                      let (v2, mem2) = eval env mem1 e2 in
                      match (v1, v2) with
                      | (Num n1, Num n2) -> if (n1==n2) then (Bool true, mem2) else (Bool false, mem2)
                      | (Bool b1, Bool b2) -> if (b1 == b2) then (Bool true, mem2) else (Bool false, mem2)
                      | (Unit, Unit) -> (Bool true, mem2)
                      | _ -> (Bool false, mem2))
  | LESS (e1, e2) -> (let (v1, mem1) = eval env mem e1 in
                      let (v2, mem2) = eval env mem1 e2 in
                      match (v1, v2) with
                      | (Num n1, Num n2) -> (Bool (n1 < n2), mem2)
                      | _ -> raise UndefinedSemantics)
  | NOT e -> (let (v, mem2) = eval env mem e in
             match v with | Bool b -> if (b==true) then (Bool false, mem2) else (Bool true, mem2) | _ -> raise UndefinedSemantics)
  | SEQ (e1, e2) -> let (_, mem1) = eval env mem e1 in eval env mem1 e2
  | IF (e, e1, e2) -> (let (v1, mem1) = eval env mem e in 
                      match v1 with 
                        | Bool b -> if (b == true) then eval env mem1 e1 else eval env mem1 e2 
                        | _ -> raise UndefinedSemantics)
  | WHILE (e1, e2) -> (let (v1, mem1) = eval env mem e1 in
                       match v1 with 
                         | Bool b -> if (b == false) then (Unit, mem1) else let (v2, mem2) = eval env mem1 e2 in eval env mem2 (WHILE (e1, e2))
                         | _ -> raise UndefinedSemantics)
  | LETV (id, e1, e2) -> let (v1, mem1) = eval env mem e1 in 
                         let l = new_location() in
                         let env1 = extend_env (LocBind(id, l)) env in
                         let mem2 = extend_mem (l, v1) mem1 in eval env1 mem2 e2
  | LETF (id, id_list, e1, e2) -> let env1 = extend_env (ProcBind(id, (id_list, e1, env))) env in eval env1 mem e2
  | CALLV (id, exp_list) -> let (valuelist, mem1) = eval_explist env exp_list ([], mem) in 
                            let (proc_id, exp, env1) = lookup_proc_env id env in
                            let loclist = allocatemem proc_id [] in
                            let newenv = extend_env (ProcBind(id, (proc_id, exp, env1))) (extend_idlist_loclist env1 proc_id loclist) in
                            let newmem = extend_loclist_valuelist mem1 loclist valuelist in
                            eval newenv newmem exp
  | CALLR (id, ref_id) -> let reflist = get_bindlist env ref_id [] in
                          let (proc_id, exp, env1) = lookup_proc_env id env in
                          let newenv = extend_env (ProcBind(id, (proc_id, exp, env1))) (extend_idlist_bindlist env1 proc_id reflist) in
                          eval newenv mem exp
  | RECORD id_exp_list -> (match id_exp_list with 
                          | [] -> (Unit, mem)
                          | _ -> let (idlist, explist) = separate_tuple id_exp_list ([], []) in
                          let (valuelist, mem1) = eval_explist env explist ([], mem) in
                          let loclist = allocatemem idlist [] in
                          let newmem = extend_loclist_valuelist mem1 loclist valuelist in 
                          let record = combine_tuple idlist loclist [] in
                          (Record record, newmem))
  | FIELD (exp, id) -> (let (v1, mem1) = eval env mem exp in
                       match v1 with 
                         | Record record -> ((lookup_mem (lookup_record id record) mem1), mem1)
                         | _ -> raise UndefinedSemantics)
                       
  | ASSIGN (id, exp) -> let (v1, mem1) = eval env mem exp in 
                        let loc = lookup_loc_env id env in (v1, (extend_mem (loc, v1) mem1))
  | ASSIGNF (e1, id, e2) -> (let (v1, mem1) = eval env mem e1 in
                             match v1 with 
                              | Record record -> let (v2, mem2) = eval env mem1 e2 in (v2, extend_mem ((lookup_record id record), v2) mem2)
                              | _ -> raise UndefinedSemantics)
  | _ -> raise NotImplemented

let runb : exp -> value 
=fun exp -> let (v, _) = eval empty_env empty_mem exp in v;;