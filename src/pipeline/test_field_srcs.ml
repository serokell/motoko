(** Maintenance note:
    Update of the expected values could be done via [dune runtest --auto-promote].
*)

module Sexpr_set = Set.Make (struct
  type t = Wasm.Sexpr.sexpr

  let compare =
    let open Wasm.Sexpr in
    let rec compare_sexprs a b = List.compare compare_sexpr a b
    and compare_sexpr a b =
      match a, b with
      | Atom n1, Atom n2 -> String.compare n1 n2
      | Node (n1, as1), Node (n2, as2) ->
        (match String.compare n1 n2 with
        | 0 -> compare_sexprs as1 as2
        | o -> o)
      | Atom _, Node _ -> -1
      | Node _, Atom _ -> +1
    in
    compare_sexpr
end)

let rec remove_srcs =
  let open Wasm.Sexpr in
  let is_not_src = function
    | Atom _ -> true
    | Node (name, _) -> name <> "@@"
  in
  function
  | Atom _ as atom -> atom
  | Node (name, args) ->
    Node (name, List.map remove_srcs @@ List.filter is_not_src args)

let gather_let_srcs sexpr =
  let open Wasm.Sexpr in
  let srcs_of_labs name args =
    match name, args with
    | "Obj", Atom "Object" :: fields ->
      List.find_map
        (function
          | Node (lab, _typ :: _depr :: _region :: srcs) -> Some (lab, srcs)
          | _ -> None)
        fields
    | _, _ -> None
  in
  let rec go acc = function
    | Atom _ -> acc
    | Node (name, args) ->
      List.fold_left
        (fun acc arg ->
          let acc =
            match srcs_of_labs name args with
            | None -> acc
            | Some (lab, srcs) -> Sexpr_set.add (Node (lab, srcs)) acc
          in
          go acc arg)
        acc
        args
  in
  List.of_seq @@ Sexpr_set.to_seq @@ go Sexpr_set.empty sexpr

let arrange filename srcs_tbl : (module Mo_def.Arrange.S) =
  (module
    (Mo_def.Arrange.Make (struct
      let include_sources = false
      let include_type_rep = Mo_def.Arrange.With_type_rep (Some srcs_tbl)
      let include_types = true
      let include_parenthetical = false
      let include_docs = None
      let main_file = Some filename
    end)))

let show_msgs msgs = String.concat "\n" (List.map Diag.string_of_message msgs)

let show (result : (Mo_def.Syntax.prog * Mo_types.Field_sources.srcs_map) Diag.result) : unit =
  match result with
  | Error msgs -> Format.printf "Diagnostics:\n%s\n" (show_msgs msgs)
  | Ok ((prog, srcs), msgs) ->
    let filename = prog.Source.note.Mo_def.Syntax.filename in
    let module Arrange = (val arrange filename srcs) in
    Format.printf "Ok:\n";
    Format.printf "Collected sources:\n";
    List.iter
      (fun srcs -> Format.printf "%s" (Wasm.Sexpr.to_string 80 srcs))
      (gather_let_srcs @@ Arrange.prog prog);
    Format.printf "Sources table:\n";
    Seq.iter
      (fun (define, origins) ->
        Format.printf "%s:" (Source.string_of_region define);
        Seq.iter
          (fun origin -> Format.printf " %s" (Source.string_of_region origin))
          (Source.Region_set.to_seq origins);
        Format.printf "\n")
      (Mo_types.Field_sources.Srcs_map.to_seq srcs);
    match msgs with
    | [] -> ()
    | _ :: _ -> Format.printf "With diagnostics:\n%s\n" (show_msgs msgs)

let run_get_sources_test source =
  let open Diag.Syntax in
  let infer_prog prog senv async_cap : Mo_types.Field_sources.srcs_map Diag.result =
    let filename = prog.Source.note.Mo_def.Syntax.filename in
    let* _typ, sscope =
      Mo_types.Cons.session ~scope:filename (fun () ->
        Mo_frontend.Typing.infer_prog
          ~viper_mode:false
          senv
          None
          async_cap
          prog)
    in
    Diag.return sscope.fld_src_env
  in
  let filename = "test-field-srcs.mo" in
  Fun.protect
    (fun () ->
      Mo_config.Flags.typechecker_combine_srcs := true;
      show begin
        let* prog, _name = Pipeline.parse_string filename source in
        let async_cap = Mo_types.Async_cap.initial_cap () in
        let* srcs = infer_prog prog Pipeline.initial_stat_env async_cap in
        Diag.return (prog, srcs)
      end)
    ~finally:(fun () -> Mo_config.Flags.typechecker_combine_srcs := false)

let%expect_test "" =
  let s = {|actor {
  class Class1() = self {
    public func meth(_ : Int) : Nat {
      return 1
    }
  };

  class Class2() = self {
    public func meth(_ : Nat) : Int {
      return 2
    }
  };

  public func test() : async Int {
    let c1 : Class2 = Class1();
    return c1.meth(42)
  }
}
|}
  in
  run_get_sources_test s;
  [%expect {|
    Ok:
    Collected sources:
    (meth
      (@@ test-field-srcs.mo (Pos 3 16) (Pos 3 20))
      (@@ test-field-srcs.mo (Pos 9 16) (Pos 9 20))
    )
    Sources table:
    internals:10.19-10.23: internals:10.19-10.23 internals:58.17-58.21 internals:64.17-64.21 internals:70.17-70.21 internals:76.17-76.21 internals:84.17-84.21 internals:92.17-92.21 internals:105.17-105.21
    internals:56.9-56.10: internals:56.9-56.10
    internals:57.9-57.10: internals:57.9-57.10
    internals:58.17-58.21: internals:58.17-58.21
    internals:62.9-62.10: internals:62.9-62.10
    internals:63.9-63.10: internals:63.9-63.10
    internals:64.17-64.21: internals:64.17-64.21
    internals:68.9-68.10: internals:68.9-68.10
    internals:69.9-69.10: internals:69.9-69.10
    internals:70.17-70.21: internals:70.17-70.21
    internals:74.9-74.10: internals:74.9-74.10
    internals:75.9-75.10: internals:75.9-75.10
    internals:76.17-76.21: internals:76.17-76.21
    internals:82.9-82.10: internals:82.9-82.10
    internals:83.9-83.10: internals:83.9-83.10
    internals:84.17-84.21: internals:84.17-84.21
    internals:90.10-90.18: internals:90.10-90.18
    internals:91.9-91.10: internals:91.9-91.10
    internals:92.17-92.21: internals:92.17-92.21
    internals:103.10-103.18: internals:103.10-103.18
    internals:104.9-104.10: internals:104.9-104.10
    internals:105.17-105.21: internals:105.17-105.21
    internals:305.4-305.11: internals:305.4-305.11 internals:372.6-372.13
    internals:306.4-306.12: internals:306.4-306.12 internals:373.6-373.14
    internals:310.21-310.23: internals:310.21-310.23
    internals:310.57-310.62: internals:310.57-310.62
    internals:372.6-372.13: internals:372.6-372.13
    internals:373.6-373.14: internals:373.6-373.14
    internals:408.15-408.26: internals:408.15-408.26
    internals:409.15-409.32: internals:409.15-409.32
    internals:410.5-410.16: internals:410.5-410.16
    internals:411.5-411.23: internals:411.5-411.23
    internals:412.5-412.22: internals:412.5-412.22
    internals:413.5-413.23: internals:413.5-413.23
    internals:418.4-418.8: internals:418.4-418.8 internals:477.14-477.18
    internals:419.4-419.11: internals:419.4-419.11
    internals:423.3-423.26: internals:423.3-423.26 internals:482.11-482.34
    internals:428.5-428.20: internals:428.5-428.20
    internals:429.7-429.15: internals:429.7-429.15 internals:447.16-447.24
    internals:430.7-430.30: internals:430.7-430.30 internals:466.55-466.78
    internals:431.18-431.29: internals:431.18-431.29
    internals:432.5-432.17: internals:432.5-432.17
    internals:433.7-433.11: internals:433.7-433.11
    internals:434.10-434.17: internals:434.10-434.17 internals:467.11-467.18
    internals:435.10-435.19: internals:435.10-435.19 internals:473.11-473.20
    internals:436.10-436.17: internals:436.10-436.17 internals:484.12-484.19
    internals:438.7-438.18: internals:438.7-438.18
    internals:439.7-439.18: internals:439.7-439.18
    internals:440.7-440.10: internals:440.7-440.10
    internals:441.7-441.30: internals:441.7-441.30
    internals:447.8-447.11: internals:447.8-447.11
    internals:447.16-447.24: internals:447.16-447.24
    internals:448.8-448.15: internals:448.8-448.15
    internals:449.8-449.17: internals:449.8-449.17
    internals:450.8-450.15: internals:450.8-450.15
    internals:451.8-451.32: internals:451.8-451.32
    internals:451.37-451.60: internals:451.37-451.60
    internals:451.86-451.94: internals:451.86-451.94
    internals:466.55-466.78: internals:466.55-466.78
    internals:467.11-467.18: internals:467.11-467.18
    internals:470.11-470.18: internals:470.11-470.18
    internals:473.11-473.20: internals:473.11-473.20
    internals:477.14-477.18: internals:477.14-477.18
    internals:482.11-482.34: internals:482.11-482.34
    internals:484.12-484.19: internals:484.12-484.19
    internals:487.32-487.55: internals:487.32-487.55
    internals:488.12-488.19: internals:488.12-488.19
    internals:540.15-540.25: internals:540.15-540.25
    internals:540.27-540.35: internals:540.27-540.35
    internals:541.21-541.31: internals:540.15-540.25 internals:541.21-541.31
    internals:541.35-541.43: internals:540.27-540.35 internals:541.35-541.43
    internals:555.16-555.22: internals:555.16-555.22 internals:609.37-609.43
    internals:555.38-555.40: internals:555.38-555.40
    internals:555.48-555.53: internals:555.48-555.53
    internals:555.64-555.67: internals:555.64-555.67
    internals:555.86-555.89: internals:555.86-555.89 internals:565.17-565.20 internals:609.60-609.63 internals:612.55-612.58 internals:670.45-670.48 internals:697.19-697.22
    internals:555.100-555.104: internals:555.100-555.104 internals:609.72-609.76 internals:613.34-613.38 internals:671.24-671.28 internals:688.36-688.40 internals:697.35-697.39
    internals:565.17-565.20: internals:565.17-565.20
    internals:609.37-609.43: internals:609.37-609.43
    internals:609.60-609.63: internals:609.60-609.63
    internals:609.72-609.76: internals:609.72-609.76
    internals:612.55-612.58: internals:612.55-612.58
    internals:613.34-613.38: internals:613.34-613.38
    internals:670.45-670.48: internals:670.45-670.48
    internals:671.24-671.28: internals:671.24-671.28
    internals:688.36-688.40: internals:688.36-688.40
    internals:697.19-697.22: internals:697.19-697.22
    internals:697.35-697.39: internals:697.35-697.39
    test-field-srcs.mo:2.9-2.15: test-field-srcs.mo:2.9-2.15
    test-field-srcs.mo:3.17-3.21: test-field-srcs.mo:3.17-3.21
    test-field-srcs.mo:8.9-8.15: test-field-srcs.mo:8.9-8.15
    test-field-srcs.mo:9.17-9.21: test-field-srcs.mo:3.17-3.21 test-field-srcs.mo:9.17-9.21
    test-field-srcs.mo:14.15-14.19: test-field-srcs.mo:14.15-14.19 |}]

let run_compare_typed_asts_test filename =
  let open Diag.Syntax in
  let load_prog () =
    let* _libs, progs, _sscope, _cache =
      Mo_types.Cons.session ~scope:filename (fun () ->
        Pipeline.load_progs_cached
          ~viper_mode:false
          ~check_actors:false
          Pipeline.parse_file
          [filename]
          Pipeline.initial_stat_env
          Mo_types.Type.Env.empty)
    in
    let prog, _deps, sscope = List.hd progs in
    Diag.return (prog, sscope.fld_src_env)
  in
  (* Ensure turning sources on will not change the AST. *)
  let* prog_no_combine, srcs_no_combine = load_prog () in
  let* prog_combine, srcs_combine =
    Fun.protect
      (fun () ->
        Mo_config.Flags.typechecker_combine_srcs := true;
        load_prog ())
      ~finally:(fun () -> Mo_config.Flags.typechecker_combine_srcs := false)
  in
  let module Arrange_no_combine = (val arrange filename srcs_no_combine) in
  let module Arrange_combine = (val arrange filename srcs_combine) in
  (* AST should match (modulo the field sources). *)
  let sexpr_prog_no_combine = remove_srcs @@ Arrange_no_combine.prog prog_no_combine in
  let sexpr_prog_combine = remove_srcs @@ Arrange_combine.prog prog_combine in
  if sexpr_prog_no_combine <> sexpr_prog_combine then
    failwith
      (Format.sprintf
        "Testing %s failed with an AST mismatch:\nAST 1:\n%sAST 2:\n%s\n%!"
        filename
        (Wasm.Sexpr.to_string 80 sexpr_prog_no_combine)
        (Wasm.Sexpr.to_string 80 sexpr_prog_combine));
  Diag.return ()

let get_mo_files_from_dir dir =
  let files = Sys.readdir dir in
  List.filter_map
    (fun file ->
      if Filename.extension file = ".mo"
      then Some (Filename.concat dir file)
      else None)
    (Array.to_list files)

let run_compare_typed_asts_tests_on_dir dir =
  let run_files = get_mo_files_from_dir dir in
  List.iter (fun file -> ignore @@ run_compare_typed_asts_test file) run_files

let%test_unit "ASTs in test match with and without combining sources" =
  List.iter
    run_compare_typed_asts_tests_on_dir
    ["run"; "run-drun"; "perf"; "bench"]
