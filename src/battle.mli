open Pokemon

(** The signature for a battle that manipulates the state of Pokemon based on
    user input *)

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

val display_and_choose_move : mon -> move
(** [display_and_choose_move pokemon] displays available moves for a Pokemon and
    lets the player choose a move. *)

val calculate_damage : mon -> mon -> move -> int
(** [calculate_damage attacker defender move] calculates the damage dealt by a
    move from an attacker to a defender Pokemon. *)

val execute_move : mon -> mon -> move -> mon
(** [execute_move attacker defender move] executes a move in a battle, affecting
    the defender's HP. *)

val team_has_fainted : mon list -> bool
(** [team_has_fainted team] checks if all Pokemon in a team have fainted. *)

val remove_fainted_pokemon : mon list -> mon list
(** [remove_fainted_pokemon team] removes all Pokemon whose HP is less than or
    equal to zero from the team. *)

val choose_next_pokemon : string -> mon list -> mon
(** [choose_next_pokemon player_name team] lets the player choose the next
    Pokemon in a battle. *)

val player_turn : string -> mon -> mon list -> mon -> mon list -> mon
(** [player_turn player_name current_pokemon player_team opponent_pokemon opponent_team]
    lets the player have two options: either to switch their Pokemon or to use a
    move. *)

val battle_turn :
  string -> mon -> mon list -> string -> mon -> mon list -> mon option
(** [battle_turn player1_name player1_pokemon player1_team player2_name player2_pokemon player2_team]
    simulates a turn in a Pokémon battle, returning an option indicating the
    outcome. *)

val start_battle : string -> mon list -> string -> mon list -> mon option
(** [start_battle player1_name player1_team player2_name player2_team] starts
    the initial play between the two teams. *)
