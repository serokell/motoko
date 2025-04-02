open Mo_def
open Mo_config
open Mo_types

module ResolveImport = Resolve_import

type no_region_parse_fn = string -> (Syntax.prog * string) Diag.result
type parse_fn = Source.region -> no_region_parse_fn

val parse_file: parse_fn
val parse_string: string -> no_region_parse_fn

val print_deps: string -> unit

val check_files  : string list -> unit Diag.result
val check_files' : parse_fn -> string list -> unit Diag.result

val viper_files : string list -> (string * (Source.region -> Source.region option)) Diag.result

val stable_compatible : string -> string -> unit Diag.result

val generate_idl : string list -> Idllib.Syntax.prog Diag.result

val initial_stat_env : Scope.scope
val chase_imports : parse_fn -> Scope.scope -> Resolve_import.resolved_imports ->
  (Syntax.lib list * Scope.scope) Diag.result

val run_files           : string list -> unit option
val run_stdin_from_file : string list -> string -> Mo_values.Value.value option
val interpret_ir_files  : string list -> unit option
val run_files_and_stdin : string list -> unit option

type compile_result =
  (Idllib.Syntax.prog * Wasm_exts.CustomModule.extended_module) Diag.result

val compile_files : Flags.compile_mode -> bool -> string list -> compile_result

val resolve_flags : unit -> ResolveImport.flags
val resolved_import_name : Syntax.resolved_import Source.phrase -> string

(* For use in the language server *)

type scope_cache = Scope.t Type.Env.t

type load_result_cached =
    ( Syntax.lib list
    * (Syntax.prog * string list) list
    * Scope.t
    * scope_cache )
  Diag.result

val load_progs_cached
  :  ?viper_mode:bool
  -> ?check_actors:bool
  -> parse_fn
  -> string list
  -> Scope.t
  -> scope_cache
  -> load_result_cached

type load_result =
  (Syntax.lib list * Syntax.prog list * Scope.scope) Diag.result

val load_progs : ?viper_mode:bool -> ?check_actors:bool -> parse_fn -> string list -> Scope.scope -> load_result
