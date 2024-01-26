open Pokemon

(** The signature for the starting environment of a game. *)

val red_text : string -> string
(** Transforms the text red *)

val green_text : string -> string
(** Transforms the text green *)

val blue_text : string -> string
(** Transforms the text blue *)

val gray_text : string -> string
(** Transforms the text green *)

val purple_text : string -> string
(** Transforms the text purple *)

val yellow_text : string -> string
(** Transforms the text yellow *)

val color_of_ptype : ptype -> string -> string
(** [color_of_ptype ptype] returns a function that transforms a string into a
    colored string based on what ptype is entered. For example, and input of
    Fire returns red_text. *)

val get_player_name : string -> string
(** [get_player_name player_label] reads the name for a player. Will print
    messages regarding invalid inputs if the name is empty or only contains
    numbers *)

val string_of_ptype : ptype -> string
(** [string_of_ptype ptype] returns the string representation of a provided
    pokemon type. For example an input of Fire would return "Fire" *)

val display_pokemon_details : mon -> unit
(** [display_pokemon_details pokemon] does not return anything and prints all
    the details of a provided pokemon, including its name, type, hp, and moves *)

val display_available_pokemon : unit -> unit
(** [display_available_pokemon ()] does not return anything and prints all
    available pokemon as supplied in the file data/pokemon.json *)

val choose_starters : string -> mon list
(** [choose_starters player_name] returns a pokemon list after prompting the
    player with name [player_name] to choose 3 pokemon for their team *)

val display_team : string -> mon list -> unit
(** [display_team player_name pokemon_list] does not return anything and prints
    all the pokemon in [pokemon_list] belonging to player with name
    [player_name] *)
