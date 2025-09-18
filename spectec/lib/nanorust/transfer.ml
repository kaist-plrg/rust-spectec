open Il.Ast
open Ast
open Concrete.Ast_utils
open Util.Source

let transfer_id (id: string) =
  let value = TextV id $$$ { vid = Concrete.Value.fresh (); typ = TextT } in
  [ Term "ID"; NT value; ] #@ "id"

let transfer_tid (tid: tvar) =
  let value = TextV tid $$$ { vid = Concrete.Value.fresh (); typ = TextT } in
  [ Term "TID"; NT value; ] #@ "tid"

let transfer_tids (ts: tvar list) =
  List.map transfer_id ts
  |> wrap_list_v "tids"

let rec transfer_type (t: typ) =
  match t with
  | UnitT -> [ Term "UNITT" ] #@ "type"
  | Tvar tv -> [ NT (transfer_tid tv) ] #@ "type"
  | Fntype (ts, t) -> 
    [
      Term "FUNCT";
      NT (transfer_types ts);
      NT (transfer_type t);
    ] #@ "type"
  | Reftype t -> [ Term "REFT"; NT (transfer_type t) ] #@ "type"
  | Structtype (s, ts) ->
    [
      Term "STRUCTT";
      NT (transfer_id s);
      NT (transfer_types ts);
    ] #@ "type"
and transfer_types (ts : typ list) =
  List.map transfer_type ts
  |> wrap_list_v "types"

let rec transfer_term (e: term) =
  match e with
  | Unit -> [ Term "UNIT" ] #@ "term"
  | Var x -> [ NT (transfer_id x) ] #@ "term"
  | Let (x, v, e) ->
    [
      Term "LET";
      NT (transfer_id x);
      Term "=";
      NT (transfer_term v);
      Term "IN";
      NT (transfer_term e);
    ] #@ "term"
  | Assign (lv, e) ->
    [
      Term "ASSIGN";
      NT (transfer_term lv);
      Term "=";
      NT (transfer_term e);
    ] #@ "term"
  | Sequence (e1, e2) ->
    [
      NT (transfer_term e1);
      Term ";";
      NT (transfer_term e2);
    ] #@ "term"
  | Ref e -> [ Term "REF"; NT (transfer_term e) ] #@ "term"
  | Deref e -> [ Term "DEREF"; NT (transfer_term e) ] #@ "term"
  | App (e, es) ->
    [
      Term "APP";
      NT (transfer_term e);
      Term "(";
      NT (transfer_terms es);
      Term ")";
    ] #@ "term"
  | Typecast (e, t) ->
    [
      Term "TYPECAST";
      NT (transfer_term e);
      Term "AS";
      NT (transfer_type t);
    ] #@ "term"
  | StructExpr (s, elems) ->
    [
      Term "STRUCT";
      NT (transfer_id s);
      NT (transfer_structElems elems);
    ] #@ "term"
  | Member (e, x) ->
    [
      Term "MEMBER";
      NT (transfer_term e);
      NT (transfer_id x);
    ] #@ "term"
and transfer_terms (es: term list) =
  List.map transfer_term es
  |> wrap_list_v "terms"
and transfer_structElem (elem: string * term) =
  let (s, e) = elem in
  [
    NT (transfer_id s);
    Term ":";
    NT (transfer_term e);
  ] #@ "structElem"
and transfer_structElems (elems: (string * term) list) =
  List.map transfer_structElem elems
  |> wrap_list_v "structElems"

let transfer_varType (vt: string * typ) =
  let (s, t) = vt in
  [
    NT (transfer_id s);
    Term ":";
    NT (transfer_type t);
  ]
  #@ "varType"

let transfer_varTypes (vts: (string * typ) list) =
  List.map transfer_varType vts
  |> wrap_list_v "varTypes"

let transfer_bound (beta: trait_bound) =
  let (d, ts) = beta in
  [
    NT (transfer_id d);
    Term "<";
    NT (transfer_types ts);
    Term ">";
  ] #@ "bound"

let transfer_cons (cons: trait_const) =
  let (t, beta) = cons in
  [
    NT (transfer_type t);
    Term ":";
    NT (transfer_bound beta);
  ] #@ "cons"

let transfer_conss (conss: trait_const list) =
  List.map transfer_cons conss
  |> wrap_list_v "conss"

let transfer_func (func: func) =
  let (f, ts, params, t, conss, e) = func in
  (f, ts, params, t,  conss, e) |> ignore;
  [
    Term "FN";
    NT (transfer_id f);
    NT (transfer_tids ts);
    NT (transfer_varTypes params);
    Term "->";
    NT (transfer_type t);
    Term "WHERE";
    NT (transfer_conss conss);
    Term "{";
    NT (transfer_term e);
    Term "}";
  ] #@ "func"

let transfer_typeScheme (scheme: type_scheme) =
  let (tvars, conss, t) = scheme in
  [
    Term "FORALL";
    NT (transfer_tids tvars);
    Term ".";
    NT (transfer_conss conss);
    Term "->";
    NT (transfer_type t);
  ] #@ "typeScheme"

let transfer_item (item: item) = match item with
  | Fun f -> transfer_func f
  | Struct (s, tvars, elemTypes) ->
    [
      Term "STRUCT";
      NT (transfer_id s);
      NT (transfer_tids tvars);
      Term "{";
      NT (transfer_varTypes elemTypes);
      Term "}";
    ] #@ "item"
  | Trait (d, tvars, conss, f, sigma) ->
    [
      Term "TRAIT";
      NT (transfer_id d);
      NT (transfer_tids tvars);
      Term "WHERE";
      NT (transfer_conss conss);
      Term "{";
      NT (transfer_id f);
      Term ":";
      NT (transfer_typeScheme sigma);
      Term "}";
    ] #@ "item"
  | Impl (tvars, d, typs, t, conss, func) ->
    [
      Term "IMPL";
      NT (transfer_tids tvars);
      NT (transfer_id d);
      NT (transfer_types typs);
      Term "FOR";
      NT (transfer_type t);
      Term "WHERE";
      NT (transfer_conss conss);
      Term "{";
      NT (transfer_func func);
      Term "}";
    ] #@ "item"

let transfer_pgm (pgm: pgm) =
  let items = List.map transfer_item pgm in
  items |> wrap_list_v "pgm"