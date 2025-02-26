(* Copyright 2019 contributors

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)
(* NOTE: This file was modified from [graph.ml] from the LSP server for the LIGO project. *)

module Make (Ord : Map.OrderedType) : sig
  (** The type of nodes in the graph. *)
  type vertex = Ord.t

  (** The abstract graph type. *)
  type t

  (** A graph with no nodes. *)
  val empty : t

  (** Check if the graph contains the given vertex. *)
  val mem : vertex -> t -> bool

  (** Add a node to the graph. *)
  val vertex : vertex -> t -> t

  (** Add various nodes to the graph. *)
  val vertices : vertex list -> t -> t

  (** Add an edge going from the first to the second node to the graph. *)
  val edge : vertex -> vertex -> t -> t

  (** Add various edges each going from the first to the second node to the
      graph. *)
  val from_assoc : (vertex * vertex) list -> t -> t

  (** Add various edges each going from the first to every list of the second
      nodes to the graph. *)
  val from_assocs : (vertex * vertex list) list -> t -> t

  (** Convert the graph to a list of nodes, each going from the first node to
      the list of second nodes. *)
  val to_assocs : t -> (vertex * vertex list) list

  (** Extract the list of nodes of the graph. *)
  val to_vertices : t -> vertex list

  (** Extract every incident child of a node, if found in the graph. *)
  val post : vertex -> t -> vertex list option

  (** Extract the subgraph of every reachable node, if found in the graph. *)
  val reachable : vertex -> t -> t option

  (** Reverse all the edges of the input graph. *)
  val transpose : t -> t

  (** Overlay a graph on top of another, performing their union. *)
  val overlay : t -> t -> t

  (** Create a double-edged graph from the input. *)
  val to_undirected : t -> t

  (** Get the Weakly Connected Components of the input graph. *)
  val wcc : t -> t list

  (** Pretty-print the graph, given a way to pretty-print a node. *)
  val pp : (Format.formatter -> vertex -> unit) -> Format.formatter -> t -> unit
end
