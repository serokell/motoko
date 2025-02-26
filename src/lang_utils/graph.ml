(* Copyright 2019 contributors

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)
(* NOTE: This file was modified from [graph.ml] from the LSP server for the LIGO project. *)

let ( <@ ) f g x = f (g x)

module type S = sig
  type vertex

  type t
  val empty : t
  val mem : vertex -> t -> bool
  val vertex : vertex -> t -> t
  val vertices : vertex list -> t -> t
  val edge : vertex -> vertex -> t -> t
  val from_assoc : (vertex * vertex) list -> t -> t
  val from_assocs : (vertex * vertex list) list -> t -> t
  val to_assocs : t -> (vertex * vertex list) list
  val to_vertices : t -> vertex list
  val post : vertex -> t -> vertex list option
  val reachable : vertex -> t -> t option
  val transpose : t -> t
  val overlay : t -> t -> t
  val to_undirected : t -> t
  val wcc : t -> t list
  val pp : (Format.formatter -> vertex -> unit) -> Format.formatter -> t -> unit
end

module Make (Ord : Map.OrderedType) : S with type vertex = Ord.t = struct
  module Node_map = Map.Make (Ord)
  module Node_set = Set.Make (Ord)

  type vertex = Ord.t
  type t = Node_set.t Node_map.t

  let empty = Node_map.empty

  let vertex node =
    Node_map.update node (function
      | None -> Some Node_set.empty
      | Some child -> Some child)

  let vertices vs g = List.fold_left (Fun.flip vertex) g vs
  let mem = Node_map.mem

  let edge from to' =
    Node_map.update
      from
      (function
        | None -> Some (Node_set.singleton to')
        | Some child -> Some (Node_set.add to' child))
    <@ vertex to'

  let from_assoc vs g = List.fold_left (fun g (v1, v2) -> edge v1 v2 g) g vs

  let from_assocs edges g =
    List.fold_left
      (fun g (node, nodes) ->
        List.fold_left (fun g v -> edge node v g) (vertex node g) nodes)
      g
      edges

  let to_assocs =
    List.map (fun (parent, children) ->
      parent, List.of_seq (Node_set.to_seq children))
    <@ List.of_seq
    <@ Node_map.to_seq

  let to_vertices = List.map fst <@ to_assocs
  let post node =
    Option.map (List.of_seq <@ Node_set.to_seq) <@ Node_map.find_opt node

  (** Internal: exposed implementation of [reachable] that performs no check
      that [node] is in [g], and returns the set of visited vertices, taking the
      initial set of [vis]ited nodes and [acc]umulated reachability graph. *)
  let rec reachable_impl (vis : Node_set.t) (acc : t) (node : vertex) (g : t)
      : Node_set.t * t
    =
    if Node_set.mem node vis
    then vis, acc
    else
      Node_set.fold
        (fun child (vis, acc) ->
          reachable_impl vis (edge node child acc) child g)
        (Node_map.find node g)
        (Node_set.add node vis, acc)

  let reachable node g =
    if mem node g
    then Some (snd @@ reachable_impl Node_set.empty empty node g)
    else None

  let transpose =
    Node_map.fold
      (fun parent children acc ->
        Node_set.fold (fun child acc -> edge child parent acc) children acc)
      empty

  let overlay = Node_map.union (fun _parent l r -> Some (Node_set.union l r))
  let to_undirected g = overlay g (transpose g)

  let wcc g =
    let double_edge_g = to_undirected g in
    snd
    @@ Node_map.fold
         (fun parent _children (vis, acc) ->
           if Node_set.mem parent vis
           then vis, acc
           else
             let vis, g' = reachable_impl vis empty parent double_edge_g in
             let g' =
               Node_map.mapi (fun parent _children -> Node_map.find parent g) g'
             in
             vis, g' :: acc)
         g
         (Node_set.empty, [])

  let pp pp_node ppf =
    let pp_list pp_elt ppf =
      let rec go ppf = function
        | [] -> ()
        | [ hd ] -> Format.fprintf ppf "%a" pp_elt hd
        | hd :: tl -> Format.fprintf ppf "%a; %a" pp_elt hd go tl
      in
      Format.fprintf ppf "[%a]" go
    in
    Node_map.iter (fun parent children ->
      Format.fprintf
        ppf
        "* %a -> %a\n"
        pp_node
        parent
        (pp_list pp_node)
        (List.of_seq (Node_set.to_seq children)))
end
