(* Support for orthogonal persistence using stable heap and in-place memory upgrades 
   without serialization or deserialization to secondary stable memory. *)

(*
  // The first type denotes the stable actor object.
  // Non-primitive types are referred to by a positive index according to the type table.
  // Primitive types are encoded by negative indices.
  // All numbers (type indices etc.) are encoded as little endian i32.
  <type_table> ::= length:i32 (<type>)^length
  <type> ::= <object> | <actor> | <function> | <mutable> | <option> | <array> | <tuple> | <variant> | <none> | <any>
  <object> ::= 1l <field_list>
  <actor> ::= 2l <field_list>
  <function> ::= 3l function_sort:i32 <type_list> <type_list>
  <mutable> ::= 4l type_index:i32
  <option> ::= 5l type_index:i32
  <array> ::= 6l type_index:i32
  <tuple> ::= 7l length:i32 (type_index:i32)^length
  <variant> ::= 8l <field_list>
  <none> ::= 9l
  <any> ::= 10l
  <field_list> ::= length:i32 (<field>)^length
  <field> ::= label_hash:i32 type_index:i32
  <type_list> ::= length:i32 <type_index:i32>^length
  
  // Predefined primitive type indices
  Type      | Index
  --------- | --------
  Null      | -1l
  Bool      | -2l
  Nat       | -3l
  Nat8      | -4l
  Nat16     | -5l
  Nat32     | -6l
  Nat64     | -7l
  Int       | -8l
  Int8      | -9l
  Int16     | -10l
  Int32     | -11l
  Int64     | -12l
  Float     | -13l
  Char      | -14l
  Text      | -15l
  Blob      | -16l
  Principal | -17l


  // Function sort
  Sort      | Encoding
  --------- | ---------
  Query     | 1l
  Write     | 2l
  Composite | 3l
*)

open Mo_types

let encode_i32 number =
  let buffer = Buffer.create 4 in
  Buffer.add_int32_le buffer number;
  Buffer.contents buffer

let list_to_string list = List.fold_left (^) "" list

module TypeTable = struct
  let empty = []

  let length table = List.length table

  let index_of table typ =
    let rec get_index table index =
      match table with
      | [] -> assert false
      | current::remainder ->
          if Type.eq current typ then index
          else get_index remainder (Int.add index 1) in
    get_index table 0

  let contains_type table typ =
    match List.find_opt (fun entry -> Type.eq entry typ) table with
    | Some _ -> true
    | None -> false

  let add_type table typ =
    let open Type in
    match typ with
    | Prim _ -> table
    | Con _ -> assert false
    | _ -> List.append table [typ]
end

let rec resolve_generics typ type_arguments =
  let open Type in
  match typ with
  | Var (_, index) -> 
      List.nth type_arguments index
  | Obj (sort, field_list) ->
      Obj (sort, (resolve_field_list field_list type_arguments))
  | Func (sort, control, bindings, parameter_list, return_list) ->
      let resolved_parameters = resolve_type_list parameter_list type_arguments in
      let resolved_returns = resolve_type_list return_list type_arguments in
      Func (sort, control, bindings, resolved_parameters, resolved_returns)
  | Variant field_list ->
      Variant (resolve_field_list field_list type_arguments)
  | Array element_type ->
      Array (resolve_generics element_type type_arguments)
  | Opt optional_type ->
      Opt (resolve_generics optional_type type_arguments)
  | Tup type_list ->
      Tup (resolve_type_list type_list type_arguments)
  | Mut mutable_type ->
      Mut (resolve_generics mutable_type type_arguments)
  | Con _ ->
      promote typ
  | Prim _ | Any | Non -> typ
  | _ ->
    assert false

and resolve_field_list field_list type_arguments =
  let open Type in
  let resolve_field field = 
    let resolved_field_type = resolve_generics field.typ type_arguments in
    { lab = field.lab; typ = resolved_field_type; depr = field.depr } in
  List.map resolve_field field_list

and resolve_type_list type_list type_arguments =
  List.map (fun typ -> resolve_generics typ type_arguments) type_list

let promote_type typ =
  let open Type in
  match typ with
  | Con (constructor, type_arguments) ->
    resolve_generics (promote typ) type_arguments
  | _ -> typ

let rec collect_type table old_typ =
  let typ = promote_type old_typ in
  if TypeTable.contains_type table typ then
    table
  else
    (let table = TypeTable.add_type table typ in
    let open Type in
    match typ with
    | Prim _ | Non | Any -> table
    | Obj (sort, field_list) when sort = Actor || sort = Object ->
      collect_fields table field_list
    | Func (Shared _, _, _, parameter_list, return_list) ->
      collect_types table (List.append parameter_list return_list)
    | Mut variable_type ->
      collect_type table variable_type
    | Opt optional_type ->
      collect_type table optional_type
    | Array element_type ->
      collect_type table element_type
    | Tup type_list ->
      collect_types table type_list
    | Variant field_list ->
      collect_fields table field_list
    | _ ->
      Printf.eprintf "Unsupported persistent type %s\n" (Type.string_of_typ typ);
      assert false)

and collect_fields table field_list =
  let open Type in
  let field_types = List.map (fun field -> field.typ) field_list in
  collect_types table field_types

and collect_types table type_list =
  match type_list with
  | [] -> table
  | first::remainder -> collect_types (collect_type table first) remainder
  
let encode_list encode_element list = 
  let length = Int32.of_int (List.length list) in
  let elements = List.map encode_element list in
  encode_i32 length ^ list_to_string elements

let primitive_type_index primitive_type =
  let open Type in
  match primitive_type with
  | Null -> -1l
  | Bool -> -2l
  | Nat -> -3l
  | Nat8 -> -4l
  | Nat16 -> -5l
  | Nat32 -> -6l
  | Nat64 -> -7l
  | Int -> -8l
  | Int8 -> -9l
  | Int16 -> -10l
  | Int32 -> -11l
  | Int64 -> -12l
  | Float -> -13l
  | Char -> -14l
  | Text -> -15l
  | Blob -> -16l
  | Principal -> -17l
  | Error -> assert false (* non-stable type *)

let type_index table typ =
  let open Type in
  let typ = promote_type typ in
  match typ with
  | Prim primitive_type -> primitive_type_index primitive_type
  | _ -> Int32.of_int (TypeTable.index_of table typ)

let encode_field table field =
  let open Type in
  let field_hash = Hash.hash field.lab in
  encode_i32 field_hash ^ 
  encode_i32 (type_index table field.typ)

let encode_type_reference table typ =
  encode_i32 (type_index table typ)

let encode_type_list table type_list =
  encode_list (encode_type_reference table) type_list

let function_sort_code sort =
  let open Type in
  match sort with
  | Query -> 1l
  | Write -> 2l
  | Composite -> 3l
  
let encode_complex_type table typ =
  let open Type in
  match typ with
  | Prim _ -> assert false
  | Obj (Object, field_list) -> 
    encode_i32 1l ^ 
    encode_list (encode_field table) field_list
  | Obj (Actor, field_list) -> 
    encode_i32 2l ^ 
    encode_list (encode_field table) field_list
  | Func (Shared sort, _, _, parameter_list, return_list) ->
    encode_i32 3l ^ 
    encode_i32 (function_sort_code sort) ^
    encode_type_list table parameter_list ^
    encode_type_list table return_list
  | Mut variable_type ->
    encode_i32 4l ^
    encode_i32 (type_index table variable_type)
  | Opt optional_type ->
    encode_i32 5l ^
    encode_i32 (type_index table optional_type)
  | Array element_type ->
    encode_i32 6l ^
    encode_i32 (type_index table element_type)
  | Tup type_list ->
    encode_i32 7l ^
    encode_type_list table type_list
  | Variant field_list ->
    encode_i32 8l ^
    encode_list (encode_field table) field_list
  | Non ->
    encode_i32 9l
  | Any ->
    encode_i32 10l
  | _ -> assert false

let encode_type_table table =
  encode_list (encode_complex_type table) table

let unwrap_optional typ =
  let open Type in
  match typ with
  | Opt inner_type -> inner_type
  | _ -> assert false

(* Encode the stable type to enable a memory compatibility check on upgrade. *)
(* See `persistence::compatibility` in the runtime system for the encoding format. *)
let encode_stable_type (stable_type: Type.typ) : string =
  let open Type in 
  match stable_type with
  | Obj (Memory, field_list) -> 
      let unwrap_field field = {lab = field.lab; typ = unwrap_optional field.typ; depr = field.depr} in
      let stable_fields = List.map unwrap_field field_list in
      let stable_actor = Obj (Object, stable_fields) in
      let table = collect_type TypeTable.empty stable_actor in
      encode_type_table table
  | _ -> assert false