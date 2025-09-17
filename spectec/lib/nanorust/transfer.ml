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
  | Tvar _ -> failwith "TODO"
  | Fntype _ -> failwith "TODO"
  | Reftype _ -> failwith "TODO"
  | Structtype _ -> failwith "TODO"
and transfer_types (ts : typ list) =
  List.map transfer_type ts
  |> wrap_list_v "types"

let transfer_term (e: term) =
  match e with
  | Unit -> [ Term "UNIT" ] #@ "term"
  | _ -> failwith "TODO: term"

let transfer_varType (vt: string * typ) =
  let (s, t) = vt in
  [
    NT (transfer_id s);
    Term "`:";
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
    Term "`<";
    NT (transfer_types ts);
    Term ">";
  ] #@ "bound"

let transfer_cons (cons: trait_const) =
  let (t, beta) = cons in
  [
    NT (transfer_type t);
    Term "`:";
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
    Term "`->";
    NT (transfer_type t);
    Term "WHERE";
    NT (transfer_conss conss);
    Term "`{";
    NT (transfer_term e);
    Term "}";
  ] #@ "func"

let transfer_item (item: item) = match item with
  | Fun f ->
    let f = transfer_func f in
    [ NT f ] #@ "func"
  | Struct (_, _, _) -> failwith "TODO : Struct"
  | Trait (_, _, _, _, _) -> failwith "TODO : Trait"
  | Impl (_, _, _, _, _, _) -> failwith "TODO : Impl"

let transfer_pgm (pgm: pgm) =
  let items = List.map transfer_item pgm in
  items |> wrap_list_v "pgm"