type tvar = string
type tvars = tvar list                (* generic type list (bar T) *)

type typ =
  | UnitT                             (* unit type `()` *)
  | Tvar of tvar                      (* type variable T *)
  | Fntype of typ list * typ          (* function type fn(t list) -> t *)
  | Reftype of typ                    (* reference type `&t` *)
  | Structtype of string * typ list   (* struct type S<t list>*)

type trait_bound = string * typ list  (* beta = D<t list> *)

type trait_const = typ * trait_bound  (* phi = t : beta *)

type type_scheme = tvars * trait_const list * typ (* type scheme `forall T. phi => typ` *)

type term =
  | Unit
  | Var of string
  | Let of string * term * term
  | Assign of term * term             (* lv := e *)
  | Sequence of term * term
  | Ref of term                       (* &lv *)
  | Deref of term
  | App of term * term list
  | Typecast of term * typ
  | StructExpr of string * (string * term) list
  | Member of term * string

type func = string * tvars * (string * typ) list * typ * trait_const list * term

type item =
  | Struct of string * tvars * (string * typ) list
  | Fun of func
  | Trait of string * tvars * trait_const list * string * type_scheme
  | Impl of tvars * string * typ list * typ * trait_const list * func

type pgm = item list