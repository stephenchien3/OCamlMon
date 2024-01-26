open Game
open Pokemon
open Battle
open Start

(** The signature for the main executable of the the game. *)

val red_text : string -> string
(** Transforms the text red *)

val green_text : string -> string
(** Transforms the text green *)

val blue_text : string -> string
(** Transforms the text blue *)

val yellow_text : string -> string
(** Transforms the text yellow *)
val color_of_ptype : ptype -> string -> string
(** [color_of_ptype ptype] returns a function that transforms a string into a
    colored string based on what ptype is entered. For example, and input of
    Fire returns red_text. *)
