open Mo_def
open Mo_types

open Syntax
open Source

module T = Type

(* a simple effect analysis to annote expressions as Triv(ial) (await-free) or Await (containing unprotected awaits) *)

(* in future we could merge this with the type-checker
   but I prefer to keep it mostly separate for now *)

let max_eff e1 e2 =
  match e1,e2 with
  | T.Triv,T.Triv -> T.Triv
  | _ , T.Await -> T.Await
  | T.Await,_ -> T.Await

let max_effs = List.fold_left max_eff T.Triv
let map_max_effs f l = max_effs (List.map f l)

let typ phrase = phrase.note.note_typ

let eff phrase = phrase.note.note_eff

let is_triv phrase  =
  eff phrase = T.Triv

let is_shared_func exp =
  T.(match typ exp with
    | Func (Shared _, _, _, _, _) -> true
    | _ -> false)

let is_local_async_func exp =
 T.(match typ exp with
   | Func (Local, Returns,
      { sort = Scope; _ }::_,
       _,
      [Async (Fut, Var (_ ,0), _)]) -> true
   | _ -> false)

let is_async_call exp1 inst exp2 =
   is_shared_func exp1 ||
   is_local_async_func exp1

let effect_exp (exp:Syntax.exp) : T.eff = eff exp

(* infer the effect of an expression, assuming all sub-expressions are correctly effect-annotated es *)
let rec infer_effect_exp (exp:Syntax.exp) : T.eff =
  match exp.it with
  | CallE (exp1, inst, exp2) when is_async_call exp1 inst exp2 ->
    T.Await
  | PrimE _
  | VarE _
  | LitE _
  | ImportE _
  | FuncE _ ->
    T.Triv
  | ActorUrlE exp1
  | UnE (_, _, exp1)
  | ShowE (_, exp1)
  | FromCandidE exp1
  | ProjE (exp1, _)
  | OptE exp1
  | DoOptE exp1
  | BangE exp1
  | TagE (_, exp1)
  | DotE (exp1, _)
  | NotE exp1
  | OldE (exp1)
  | AssertE (_, exp1)
  | LabelE (_, _, exp1)
  | BreakE (_, exp1)
  | RetE exp1
  | AnnotE (exp1, _)
  | IgnoreE exp1
  | LoopE (exp1, None) ->
    effect_exp exp1
  | BinE (_, exp1, _, exp2)
  | IdxE (exp1, exp2)
  | RelE (_, exp1, _, exp2)
  | AssignE (exp1, exp2)
  | CallE (exp1, _, exp2)
  | AndE (exp1, exp2)
  | OrE (exp1, exp2)
  | ImpliesE (exp1, exp2)
  | WhileE (exp1, exp2)
  | LoopE (exp1, Some exp2)
  | ForE (_, exp1, exp2) ->
    let t1 = effect_exp exp1 in
    let t2 = effect_exp exp2 in
    max_eff t1 t2
  | DebugE exp1 ->
    effect_exp exp1
  | ToCandidE exps
  | TupE exps
  | ArrayE (_, exps) ->
    map_max_effs effect_exp exps
  | BlockE decs ->
    map_max_effs effect_dec decs
  | ObjBlockE (sort, _, dfs) ->
    infer_effect_dec_fields dfs
  | ObjE (bases, efs) ->
    let bases = map_max_effs effect_exp bases in
    let fields = infer_effect_exp_fields efs in
    max_eff fields bases
  | IfE (exp1, exp2, exp3) ->
    map_max_effs effect_exp [exp1; exp2; exp3]
  | SwitchE (exp1, cases) ->
    let e1 = effect_exp exp1 in
    let e2 = effect_cases cases in
    max_eff e1 e2
  | AsyncE (T.Fut, _, _) ->
    T.Await
  | AsyncE (T.Cmp, _, _) ->
    T.Triv
  | ThrowE _
  | TryE _
  | AwaitE _ ->
    T.Await (* TBR: perhaps we should rename the effect *)

and effect_cases cases =
  match cases with
  | [] ->
    T.Triv
  | {it = {pat; exp}; _}::cases' ->
    let e = effect_exp exp in
    max_eff e (effect_cases cases')

and infer_effect_dec_fields dfs =
  List.fold_left (fun e (df : dec_field) -> max_eff e (effect_dec df.it.dec)) T.Triv dfs

and infer_effect_exp_fields efs =
  List.fold_left (fun e (ef : exp_field) -> max_eff e (effect_exp ef.it.exp)) T.Triv efs

and effect_dec dec =
  dec.note.note_eff

and infer_effect_dec dec =
  match dec.it with
  | LetD (_, e, Some f) ->
    max_eff (effect_exp e) (effect_exp f)
  | ExpD e
  | LetD (_, e, None)
  | VarD (_, e) ->
    effect_exp e
  | TypD _
  | ClassD _ ->
    T.Triv
