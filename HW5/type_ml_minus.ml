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

exception TypeError

type typ = 
    TyUnit 
  | TyInt 
  | TyBool 
  | TyFun of typ * typ 
  | TyList of typ
  | TyVar of tyvar
and tyvar = string
type typenv = (var * typ) list
type typeqn = (typ * typ) list
type substit = (typ * typ) list


(* You can invoke "fresh_tyvar()" to generate a fresh type variable *)
let tyvar_num = ref 0
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec lookuptypenv : typenv -> var -> typ
= fun typenv var ->
  match typenv with
  |[] -> raise TypeError
  |hd::tl -> let (var1, typ1) = hd in if var = var1 then typ1 else lookuptypenv tl var 

let rec gentypeqn : typenv -> exp -> typ -> typeqn
= fun typenv exp typ ->
  match exp with
  | UNIT -> [(typ, TyUnit)]
  | TRUE -> [(typ, TyBool)]
  | FALSE -> [(typ, TyBool)]
  | CONST n -> [(typ, TyInt)]
  | VAR var -> let vartype = lookuptypenv typenv var in [(typ, vartype)]
  | ADD (e1, e2) -> (typ, TyInt) :: (gentypeqn typenv e1 TyInt) @ (gentypeqn typenv e2 TyInt)
  | SUB (e1, e2) -> (typ, TyInt) :: (gentypeqn typenv e1 TyInt) @ (gentypeqn typenv e2 TyInt)
  | MUL (e1, e2) -> (typ, TyInt) :: (gentypeqn typenv e1 TyInt) @ (gentypeqn typenv e2 TyInt)
  | DIV (e1, e2) -> (typ, TyInt) :: (gentypeqn typenv e1 TyInt) @ (gentypeqn typenv e2 TyInt)
  | EQUAL (e1, e2) -> let newtyp = fresh_tyvar() in (typ, TyBool) :: (gentypeqn typenv e1 newtyp) @ (gentypeqn typenv e2 newtyp)
  | LESS (e1, e2) -> (typ, TyBool) :: (gentypeqn typenv e1 TyInt) @ (gentypeqn typenv e2 TyInt)
  | NOT e -> (typ, TyBool) :: (gentypeqn typenv e TyBool)
  | NIL -> [(typ, TyList(fresh_tyvar()))] (* 쓰읍*)
  | CONS (e1, e2) -> let newtyp = fresh_tyvar() in (typ, TyList newtyp) :: (gentypeqn typenv e1 newtyp) @ (gentypeqn typenv e2 (TyList newtyp))     
  | APPEND (e1, e2) -> let newtyp = fresh_tyvar() in (typ, TyList newtyp) :: (gentypeqn typenv e1 (TyList newtyp)) @ (gentypeqn typenv e2 (TyList newtyp))
  | HEAD e -> let newtyp = fresh_tyvar() in (typ, newtyp) :: (gentypeqn typenv e (TyList newtyp))
  | TAIL e -> let newtyp = fresh_tyvar() in (typ, TyList newtyp) :: (gentypeqn typenv e (TyList newtyp))
  | ISNIL e -> let newtyp = fresh_tyvar() in (typ, TyBool) :: (gentypeqn typenv e (TyList newtyp)) (*쓰읍*)
  | IF (e1, e2, e3) -> (gentypeqn typenv e2 typ) @ (gentypeqn typenv e3 typ) @ (gentypeqn typenv e1 TyBool)
  | LET (var, e1, e2) -> let newtyp = fresh_tyvar() in (gentypeqn((var, newtyp)::typenv) e2 typ) @ (gentypeqn typenv e1 newtyp)
  | LETREC (var1, var2, e1, e2) -> let newtyp1 = fresh_tyvar() in let newtyp2 = fresh_tyvar() in 
                                   (gentypeqn((var1, TyFun(newtyp1, newtyp2))::typenv) e2 typ) @ 
                                   (gentypeqn ([(var1, TyFun(newtyp1, newtyp2));(var2, newtyp1)]@typenv) e1 newtyp2)
  | LETMREC ((var1, var2, e1), (var3, var4, e2), e3) -> let newtyp1 = fresh_tyvar() in let newtyp2 = fresh_tyvar() in 
                                                       (gentypeqn([(var1, TyFun(newtyp1, newtyp2));(var3, TyFun(newtyp1, newtyp2))]@typenv) e3 typ) @ 
                                                       (gentypeqn ([(var1, TyFun(newtyp1, newtyp2));(var3, TyFun(newtyp1, newtyp2));(var2, newtyp1)]@typenv) e1 newtyp2) @ 
                                                       (gentypeqn ([(var1, TyFun(newtyp1, newtyp2));(var3, TyFun(newtyp1, newtyp2));(var4, newtyp1)]@typenv) e2 newtyp2) 
  | PROC (var, e) -> let newtyp1 = fresh_tyvar() in let newtyp2 = fresh_tyvar() in (typ, TyFun(newtyp1, newtyp2)) :: (gentypeqn((var, newtyp1)::typenv) e newtyp2)
  | CALL (e1, e2) -> let newtyp1 = fresh_tyvar() in let newtyp2 = fresh_tyvar() in (typ, newtyp2) :: (gentypeqn typenv e1 (TyFun(newtyp1, newtyp2))) @ (gentypeqn typenv e2 newtyp1)
  | PRINT e -> let newtyp = fresh_tyvar() in (typ, TyUnit) :: (gentypeqn typenv e newtyp)
  | SEQ (e1, e2) -> let newtyp = fresh_tyvar() in (gentypeqn typenv e2 typ) @ (gentypeqn typenv e1 newtyp)

let rec apply : typ -> substit -> typ
= fun typ substit ->
  match typ with 
  | TyVar typvar -> (match substit with 
                    | hd::tl -> let (typ1, typ2) = hd in (match typ1 with 
                                                          | TyVar typvar1 -> if typvar1 = typvar then typ2 else apply typ tl 
                                                          | _ -> apply typ tl)
                    | [] -> typ)
  | TyFun(t1, t2) -> TyFun((apply t1 substit), (apply t2 substit))
  | TyList (t) -> TyList(apply t substit)
  | _ -> typ
  
let rec alwaystrue : typ -> typ -> bool
= fun typ1 typ2 ->
  match (typ1, typ2) with
  | (TyUnit, TyUnit) -> true
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyList t1, TyList t2) -> alwaystrue t1 t2
  | (TyVar v1, TyVar v2) -> if v1 = v2 then true else false
  | (TyFun(t1, t2), TyFun(t3, t4)) -> (alwaystrue t1 t3) && (alwaystrue t2 t4)
  | _ -> false
  
let rec contradict : typ -> typ -> bool
= fun typ1 typ2 ->
  match (typ1, typ2) with
  | (TyUnit, TyUnit) -> false
  | (TyInt, TyInt) -> false
  | (TyBool, TyBool) -> false
  | (TyList t1, TyList t2) -> contradict t1 t2
  | (TyFun(t1, t2), TyFun(t3, t4)) -> (contradict t1 t3)||(contradict t2 t4)
  | (TyVar v, _) -> false
  | (_, TyVar v) -> false
  | _ -> true
  
let rec checkoccurence : typ -> typ -> bool
= fun left right ->
  match (left, right) with
  | (_, TyFun(t1, t2)) -> (checkoccurence left t1)||(checkoccurence left t2)
  | (_, TyList t) -> checkoccurence left t
  | (TyVar v1, TyVar v2) -> if v1 = v2 then true else false
  | _ -> false

let rec substitute : typ -> typ -> typ -> typ
= fun typ1 typ2 subtyp ->
  match typ1 with 
  |TyVar v1 -> (match subtyp with 
                | TyVar v -> if v1 = v then typ2 else subtyp 
                | TyFun (t1, t2) -> TyFun(substitute typ1 typ2 t1, substitute typ1 typ2 t2) 
                | TyList t -> TyList(substitute typ1 typ2 t)
                | _ -> subtyp )
  | _ -> subtyp    
  
let rec substituteall : typ -> typ -> substit -> substit
= fun typ1 typ2 substit ->
  match substit with
  | [] -> substit
  | hd::tl -> let (typ3, typ4) = hd in let newtyp = substitute typ1 typ2 typ4 in (typ3, newtyp) :: (substituteall typ1 typ2 tl)
  
let rec unifyall : typeqn -> substit -> substit
= fun typeqn substit ->
  match typeqn with
  | [] -> substit
  | hd::tl -> let (typ1, typ2) = hd in 
              let applyed1 = apply typ1 substit in
              let applyed2 = apply typ2 substit in
              if (alwaystrue applyed1 applyed2) then unifyall tl substit else 
                if (contradict applyed1 applyed2) then raise TypeError else 
                (match (applyed1, applyed2) with 
                  | (TyFun(t1, t2), TyFun(t3, t4)) -> unifyall ([(t1,t3);(t2,t4)]@tl) substit
                  | (TyList t1, TyList t2) -> unifyall ((t1,t2)::tl) substit
                  | (TyUnit, _) | (TyInt, _) | (TyBool, _) | (TyList _, _) | (TyFun _, _) -> if (checkoccurence applyed2 applyed1) then raise TypeError 
                     else let newsubstit = (substituteall applyed2 applyed1 substit)@[(applyed2, applyed1)] in unifyall tl newsubstit
                  | _ -> if (checkoccurence applyed1 applyed2) then raise TypeError
                     else let newsubstit = (substituteall applyed1 applyed2 substit)@[(applyed1, applyed2)] in unifyall tl newsubstit 
                  )
                  
let typeof : exp -> typ 
=fun exp -> 
  let typeofexp = fresh_tyvar() in 
  let typeqn = gentypeqn [] exp typeofexp in
  let substit = unifyall typeqn [] in 
  match substit with |hd::tl -> let (tyvar, typ) = hd in (match typ with 
                                                           |TyVar v -> raise TypeError
                                                           | _ -> typ) 
                     |[]-> raise TypeError;;
                     