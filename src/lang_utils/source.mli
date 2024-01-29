type pos = {file : string; line : int; column : int}
type region = {left : pos; right : pos}
type ('a, 'b) annotated_phrase = {at : region; it : 'a; mutable note: 'b}
type 'a phrase = ('a, unit) annotated_phrase

val no_pos : pos
val no_region : region

val string_of_pos : pos -> string
val string_of_region : region -> string
val yojson_of_region : region -> Yojson.Safe.t
val yojson_of_phrase : ('a -> Yojson.Safe.t) -> 'a phrase -> Yojson.Safe.t
val yojson_of_annotated_phrase : ('a -> Yojson.Safe.t) -> ('b -> Yojson.Safe.t)
  -> ('a, 'b) annotated_phrase -> Yojson.Safe.t



val span : region -> region -> region
val between : region -> region -> region

val (@@) : 'a -> region -> 'a phrase

exception ParseError of region * string
