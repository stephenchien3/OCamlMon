(** The signature of a Pokemon with names and moves. *)

type ptype =
  | Fire
  | Water
  | Grass
  | Normal

(** [ptype] is a record type to represent different types of Pokemon *)

type move = {
  move_name : string;
  base_power : int option;
  mtype : ptype;
  effects : string;
  accuracy : int;
}
(** [move] is a record type to represent moves *)

type mon = {
  name : string;
  mutable hp : int;
  ptype : ptype;
  moveset : move list;
  mutable is_poisoned : bool;
}
(** [mon] is a record type to represent Pokemon *)

val get_movename : move -> string
(** [get_movename] returns the name of the inputted move. For example,
    [get_movename scratch] returns "scratch" *)

val get_basepower : move -> int option
(** [get_basepower] returns an integer option representing the base power of the
    inputted move. Returns [None] if the move has no base power. For example,
    [get_basepower scratch] returns [Some 40] *)

val get_mtype : move -> ptype

(** [get_mtype] returns the ptype of the inputted move. For example,
    [get_mtype leech_seed] returns [Grass]*)

val get_effects : move -> string
(** [get_effects] returns the effects of the inputted move, represented by a
    string. For example, [get_effects scratch] returns ["No additional effect."] *)

val get_accuracy : move -> int
(** [get_accuracy] returns the accuracy (represented by an integer) of the
    inputted move. For example, [get_accuracy scratch] returns [100] *)

val get_hp : mon -> int
(** [get_hp] returns the health points (represented as an integer) of the
    inputted Pokemon. For example, [get_hp bulbasaur] returns [76] *)

val get_name : mon -> string
(** [get_name] returns the name (represented as a string) of the inputted
    Pokemon. For example, [get_name bulbasaur] returns ["Bulbasaur"] *)

val get_ptype : mon -> ptype
(** [get_ptype] returns the ptype of the inputted Pokemon. For example,
    [get_ptype bulbasaur] returns [Grass] *)

val get_moveset : mon -> move list
(** [get_moveset] returns the moveset (represented by a list of moves) of the
    inputted Pokemon. For example, [get_moveset bulbasaur] returns
    [[ vine_whip; tackle ]]*)

val compare_type : mon -> mon -> int
(** [compare_type] takes two inputted Pokemon and returns -1 if p1's type is
    weak against p2's type, 1 if p1's type is strong against p2's type, and 0
    otherwise. For example, [compare_type bulbasaur charmander] returns [-1]
    since the [Grass] type is weak against the [Fire] type*)

val compare_hp : mon -> mon -> int
(** [compare_hp] takes two inputted Pokemon and uses the built-in OCaml compare
    function to compare two Pokemon's health points. *)

val parse_ptype : string -> ptype
(** [parse_ptype] takes an inputted string and returns the associated ptype. For
    example, [parse_ptype "Fire"] returns [Fire]. *)

val parse_move : Yojson.Basic.t -> move
(** [parse_move] takes in a single element of the json file and converts it into
    a move. *)

val parse_move_list : string -> move list
(** [parse_move_list] takes in a json file, calls parse_move on every element of
    the json file, and returns a list of moves. *)

val find_move_by_name : move list -> string -> move
(** [find_move_by_nam] takes a list of moves and a string representing a move
    and returns the move with that name. For example,
    [find_move_by_name [ vine_whip; tackle ] "tackle"] returns [tackle] *)

val parse_pokemon : move list -> Yojson.Basic.t -> mon
(** [parse_pokemon] takes in a list of all moves and a single element of the
    Json file, and converts it into a mon. *)

val parse_pokemon_list : move list -> string -> mon list
(** [parse_pokemon_list] takes in a list of all moves and a json file, calls
    [parse_pokemon] on every element of the json file, and returns a list of
    mon. *)

val all_pokemon : mon list
(** [all_pokemon] represents a compiled list of pokemon from the json file. *)

val all_moves : move list
(** [all_moves] represents compiled list of moves from the json file. *)
