open Type
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let control = function
  | Returns -> "Returns"
  | Promises -> "Promises"
  | Replies -> "Replies"

let obj_sort = function
  | Object -> Atom "Object"
  | Actor -> Atom "Actor"
  | Module -> Atom "Module"
  | Memory -> Atom "Memory"

let func_sort = function
  | Local -> "Local"
  | Shared Write -> "Shared"
  | Shared Query -> "Shared Query"
  | Shared Composite -> "Shared Composite"

let prim = function
  | Null -> Atom "Null"
  | Bool -> Atom "Bool"
  | Nat -> Atom "Nat"
  | Nat8 -> Atom "Nat8"
  | Nat16 -> Atom "Nat16"
  | Nat32 -> Atom "Nat32"
  | Nat64 -> Atom "Nat64"
  | Int -> Atom "Int"
  | Int8 -> Atom "Int8"
  | Int16 -> Atom "Int16"
  | Int32 -> Atom "Int32"
  | Int64 -> Atom "Int64"
  | Float -> Atom "Float"
  | Char -> Atom "Char"
  | Text -> Atom "Text"
  | Blob -> Atom "Blob"
  | Error -> Atom "Error"
  | Principal -> Atom "Principal"
  | Region -> Atom "Region"

let con c = Atom (Type.string_of_con c)

let pos p =
  "Pos" $$
    [ Atom (string_of_int p.Source.line)
    ; Atom (string_of_int p.Source.column) ]

let region at =
  let filename = at.Source.left.Source.file in
  "@@" $$ [Atom filename; pos at.Source.left; pos at.Source.right]

let src {depr; region = r; srcs} =
  Atom (Option.value ~default:"" depr)
  :: region r
  :: List.of_seq (Seq.map region (Region_set.to_seq srcs))

let rec typ = function
  | Var (s, i)             -> "Var" $$ [Atom s; Atom (string_of_int i)]
  | Con (c, ts)            -> "Con" $$ (con c::List.map typ ts)
  | Prim p                 -> "Prim" $$ [prim p]
  | Obj (s, tfs)           -> "Obj" $$ [obj_sort s] @ List.map typ_field tfs
  | Array t                -> "Array" $$ [typ t]
  | Opt t                  -> "Opt" $$ [typ t]
  | Variant tfs            -> "Variant" $$ List.map typ_field tfs
  | Tup ts                 -> "Tup" $$ List.map typ ts
  | Func (s, c, tbs, at, rt) ->
    "Func" $$ [Atom (func_sort s); Atom (control c)] @
      List.map typ_bind tbs @ [ "" $$ (List.map typ at); "" $$ (List.map typ rt)]
  | Async (Fut, t1, t2)    -> "Async" $$ [typ t1; typ t2]
  | Async (Cmp, t1, t2)    -> "Async*" $$ [typ t1; typ t2]
  | Mut t                  -> "Mut" $$ [typ t]
  | Any                    -> Atom "Any"
  | Non                    -> Atom "Non"
  | Pre                    -> Atom "Pre"
  | Typ c                  -> "Typ" $$ [con c]
  | Named (n, t)            -> "Name" $$ [Atom n; typ t]

and typ_bind (tb : Type.bind) =
  tb.var $$ [typ tb.bound]

and typ_field ({lab; typ = t; src = s} : Type.field) =
  lab $$ typ t :: src s
