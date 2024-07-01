open Source
open Syntax

open Format

let marks = ref []

let pr = pp_print_string

let comma ppf () = fprintf ppf ",@ "

let semi ppf () = fprintf ppf ";@ "

let pp_info ppf info =
  match info with
  | NoInfo -> ()
  | ActorInit -> fprintf ppf "@[// translation of actor initializers @]"
  | PublicFunction x -> fprintf ppf "@[// translation of PUBLIC function %s@]" x
  | PrivateFunction x -> fprintf ppf "@[// translation of _private_ function %s@]" x

let rec pp_prog ppf p =
  match p.it with
  | is ->
   fprintf ppf "@[<v 0>%a@]"
   (pp_print_list pp_item) is

and pp_item ppf i =
  match i.it with
  | AdtI (id, params, cons) ->
    fprintf ppf "@[<2>adt %s@;%a@;%a@]"
      id.it
      pp_adt_params params
      pp_adt_cons cons
  | FieldI (id, typ) ->
    fprintf ppf "@[<2>field %s:@ %a@]"
      id.it
      pp_typ typ
  | MethodI (id, locals, rets, pres, posts, bo) ->
    marks := i.at :: !marks;
    fprintf ppf "\017@[<v 2>method %s%a@; %a@; %a@; %a@; %a@]\019"
      id.it
      pp_locals locals
      pp_returns rets
      pp_pres pres
      pp_posts posts
      pp_block_opt bo
  | InvariantI (inv_name, e) -> (* TODO: srcloc mapping *)
    fprintf ppf "@[<2>define %s($Self) (%a)@]" inv_name pp_exp e

and pp_adt_params ppf = function
  | []     -> ()
  | params ->
      fprintf ppf "[%a]"
        (pp_print_list pp_adt_param ~pp_sep:comma) params

and pp_adt_param ppf param = fprintf ppf "%s" param.it

and pp_adt_cons ppf cons =
  fprintf ppf "@[<v 2>{ %a }@]"
    (pp_print_list pp_adt_con) cons

and pp_adt_con ppf con =
  fprintf ppf "%s@[(%a)@]"
    con.con_name.it
    (pp_print_list ~pp_sep:comma pp_adt_con_field) con.con_fields

and pp_adt_con_field ppf (field_name, field_type) =
  fprintf ppf "%s : %a"
    field_name.it
    pp_typ field_type

and pp_block_opt ppf = function
  | None -> ()
  | Some seqn ->
    pp_seqn ppf seqn

and pp_seqn ppf seqn =
    let (ds, ss) = seqn.it in
    fprintf ppf "@[<v 2>{ %a@ %a @;<0 -2>}@]"
     (pp_print_list pp_decl) ds
     (pp_print_list pp_stmt) ss

and pp_decl ppf decl =
    let (id, typ) = decl.it in
    fprintf ppf "@[<v 0>var %s: %a@]"
    id.it
    pp_typ typ

and pp_binder ppf (id, typ) =
    fprintf ppf "@[%s : %a@]" id.it pp_typ typ

and pp_pres ppf exps =
   fprintf ppf "@[<v 0>%a@]" (pp_print_list pp_pre) exps

and pp_pre ppf exp =
   marks := exp.at :: !marks;
   fprintf ppf "\017@[<v 2>requires %a@]\019" pp_exp exp

and pp_posts ppf exps =
   fprintf ppf "@[<v 0>%a@]" (pp_print_list pp_post) exps

and pp_post ppf exp =
   marks := exp.at :: !marks;
   fprintf ppf "\017@[<v 2>ensures %a@]\019" pp_exp exp

and pp_local ppf (id, typ) =
  fprintf ppf "@[<2>%s: %a@]"
    id.it
    pp_typ typ

and pp_locals ppf pars =
  fprintf ppf "@[<1>(%a)@]"
    (pp_print_list ~pp_sep:comma (pp_local)) pars

and pp_returns ppf pars =
  match pars with
  | [] -> ()
  | _ ->
    fprintf ppf "@[<1> returns (%a)@]"
      (pp_print_list ~pp_sep:comma (pp_local)) pars

and pp_typ ppf t =
  match t.it with
  | IntT -> pr ppf "Int"
  | BoolT -> pr ppf "Bool"
  | RefT -> pr ppf "Ref"
  | ArrayT -> pr ppf "Array"
  | TupleT [] -> pr ppf "Tuple$0"
  | TupleT ts ->
      fprintf ppf "@[Tuple$%d[%a]@]"
        (List.length ts)
        (pp_print_list ~pp_sep:comma pp_typ) ts
  | OptionT t -> fprintf ppf "@[Option[%a]@]" pp_typ t
  | ConT(con, []) -> fprintf ppf "%s" con.it
  | ConT(con, ts) ->
      fprintf ppf "@[%s[%a]@]"
        con.it
        (pp_print_list ~pp_sep:comma pp_typ) ts

and pp_exp ppf exp =
  match exp.it with
  | LocalVar (id, _) ->
     fprintf ppf "%s" id.it
  | FldAcc fldacc ->
     pp_fldacc ppf fldacc
  | FldE s ->
     fprintf ppf "%s" s
  | CallE (m, es) ->
     fprintf ppf "@[%s(%a)@]" m (pp_print_list pp_exp ~pp_sep:comma) es
  | NotE e ->
     fprintf ppf "@[(!%a)@]" pp_exp e
  | MinusE e ->
     fprintf ppf "@[(-%a)@]" pp_exp e
  | NullLitE ->
     fprintf ppf "null"
  | BoolLitE b ->
     fprintf ppf "%s" (if b then "true" else "false")
  | IntLitE i ->
     fprintf ppf "%s" String.(of_seq (Seq.filter (fun c -> c <> '_') (to_seq (Mo_values.Numerics.Int.to_string i))))
  | AddE (e1, e2) | SubE (e1, e2) | MulE (e1, e2) | DivE (e1, e2) | ModE (e1, e2)
  | EqCmpE (e1, e2) | NeCmpE (e1, e2) | GtCmpE (e1, e2) | GeCmpE (e1, e2) | LtCmpE (e1, e2) | LeCmpE (e1, e2)
  | Implies (e1, e2) | OrE (e1, e2) | AndE (e1, e2) ->
     let op = match exp.it with
       | AddE _ -> "+" | SubE _ -> "-"
       | MulE _ -> "*" | DivE _ -> "/" | ModE _ -> "%"
       | EqCmpE _ -> "==" | NeCmpE _ -> "!="
       | GtCmpE _ -> ">" | GeCmpE _ -> ">="
       | LtCmpE _ -> "<" | LeCmpE _ -> "<="
       | Implies _ -> "==>" | OrE _ -> "||" | AndE _ -> "&&"
       | _ -> failwith "not a binary operator" in
     fprintf ppf "(%a %s %a)" pp_exp e1 op pp_exp e2
  | Old e ->
    fprintf ppf "@[old(%a)@]" pp_exp e
  | PermE p -> pp_perm ppf p
  | AccE (fldacc, perm) -> fprintf ppf "@[acc(%a,%a)@]" pp_fldacc fldacc pp_exp perm
  | ForallE (binders, exp) -> fprintf ppf "@[(forall %a :: %a)@]" (pp_print_list ~pp_sep:comma pp_binder) binders pp_exp exp
  | ExistsE (binders, exp) -> fprintf ppf "@[(exists %a :: %a)@]" (pp_print_list ~pp_sep:comma pp_binder) binders pp_exp exp
  | _ -> fprintf ppf "@[// pretty printer not implemented for node at %s@]" (string_of_region exp.at)

and pp_perm ppf perm =
  match perm.it with
  | NoP -> fprintf ppf "none"
  | FullP -> fprintf ppf "write"
  | WildcardP -> fprintf ppf "wildcard"
  | FractionalP (a, b) -> fprintf ppf "@[(%a/%a)@]" pp_exp a pp_exp b

and pp_stmt ppf stmt =
  marks := stmt.at :: !marks;
  fprintf ppf "\017%a\019;"
    pp_stmt' stmt.it

and pp_stmt' ppf = function
  | SeqnS seqn -> pp_seqn ppf seqn
  | IfS (exp1, s1, { it = ([],[]); _ }) ->
    fprintf ppf "@[<v 2>if (%a)@ %a@]"
      pp_exp exp1
      pp_seqn s1
  | IfS (exp1, s1, s2) ->
    fprintf ppf "@[<v 2>if (%a)@ %aelse@ %a@]"
      pp_exp exp1
      pp_seqn s1
      pp_seqn s2
  | WhileS (exp, invs, s) ->
    fprintf ppf "@[<v 2>while (%a)@;@[<v 0>%a@]@;%a@]"
      pp_exp exp
      (pp_print_list pp_loop_inv) invs
      pp_seqn s
  | VarAssignS (id, exp) ->
    fprintf ppf "@[<v 2>%s := %a@]"
      id.it
      pp_exp exp
  | FieldAssignS (fldacc, exp) ->
    fprintf ppf "@[<v 2>%a := %a@]"
      pp_fldacc fldacc
      pp_exp exp
  | InhaleS exp ->
    fprintf ppf "@[<v 2>inhale %a@]"
      pp_exp exp
  | ExhaleS exp ->
    fprintf ppf "@[<v 2>exhale %a@]"
      pp_exp exp
  | AssumeS exp ->
    fprintf ppf "@[<v 2>assume %a@]"
      pp_exp exp
  | AssertS exp ->
    fprintf ppf "@[<v 2>assert %a@]"
      pp_exp exp
  | PreconditionS exp ->
    fprintf ppf "@[<v 2>/*requires %a*/@]"
      pp_exp exp
  | PostconditionS exp ->
    fprintf ppf "@[<v 2>/*ensures %a*/@]"
      pp_exp exp
  | ConcurrencyS (max, exp, _) ->
    fprintf ppf "@[<v 2>/*concurrency max %s, cond: s %a*/@]"
      max
      pp_exp exp
  | MethodCallS (rs, m, args) ->
    let () = if rs != [] then
      fprintf ppf "@[%a@] := " (pp_print_list ~pp_sep:comma pp_res_var) rs
    in
    let () = fprintf ppf "@[%s(@]" m.it in
    let () = fprintf ppf "@[%a@]" (pp_print_list ~pp_sep:comma pp_exp) args in
    fprintf ppf ")"
  | LabelS lbl ->
      fprintf ppf "@[label %s@]" lbl.it
  | GotoS lbl ->
      fprintf ppf "@[goto %s@]" lbl.it

and pp_res_var ppf r = fprintf ppf "%s" r.it

and pp_fldacc ppf fldacc =
  match fldacc with
  | (exp1, id) ->
    fprintf ppf "@[(%a).%s@]" pp_exp exp1 id.it

and pp_loop_inv ppf inv =
    marks := inv.at :: !marks;
    fprintf ppf "\017invariant %a\019" pp_exp inv

let prog_mapped file prelude p =
    marks := [];
    let b = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer b in
    Format.fprintf ppf "@[%s@]@.@.@[%a@]" prelude pp_prog p;
    Format.pp_print_flush ppf ();
    let in_file { left; right } =
      let left, right = { left with file }, { right with file } in
      { left ; right } in
    let marks = ref (List.rev_map (fun loc -> loc, in_file loc) !marks, [], []) in
    let pos = ref 0 in
    let push line column = match !marks with
        | (mot, vip) :: clos, ope, don -> marks := clos, (mot, { vip with left = { vip.left with line; column } }) :: ope, don
        | _ -> assert false in
    let pop line column = match !marks with
        | clos, (mot, vip) :: ope, don -> marks := clos, ope, (mot, { vip with right = { vip.right with line; column } }) :: don
        | _ -> assert false in
    let line = ref 1 in
    let examine = function
    | '\n' -> line := !line + 1; pos := 0; '\n';
    | '\017' -> push !line !pos; '\017'
    | '\019' -> pop !line !pos; '\017'
    | a -> pos := !pos + 1; a in
    let clean = function
    | '\017' -> false
    | _ -> true in
    let b = Buffer.(of_seq Seq.(filter clean (map examine (to_seq b)))) in
    let _, _, mapping = !marks in
    let inside { left; right } other =
        left.file = other.left.file &&
        right.file = other.right.file &&
        (other.left.line, other.left.column) <= (left.line, left.column) &&
        (right.line, right.column) <= (other.right.line, other.right.column) in
    let lookup (r : Source.region) =
        let tighten prev (mot, vip) =
            if inside r vip
            then Some mot
            else prev in
        List.fold_left tighten None mapping in
    Buffer.contents b, lookup

let prog prelude p = fst (prog_mapped "" prelude p)
