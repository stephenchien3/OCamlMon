open Pokemon

let yellow_text text = "\027[33m" ^ text ^ "\027[0m"
let red_text text = "\027[31m" ^ text ^ "\027[0m"
let green_text text = "\027[32m" ^ text ^ "\027[0m"
let blue_text text = "\027[34m" ^ text ^ "\027[0m"
let gray_text text = "\027[90m" ^ text ^ "\027[0m"
let purple_text text = "\027[35m" ^ text ^ "\027[0m"

let color_of_move_type move_type =
  match move_type with
  | Fire -> red_text
  | Water -> blue_text
  | Grass -> green_text
  | Normal -> gray_text

let color_of_ptype ptype =
  match ptype with
  | Fire -> red_text
  | Water -> blue_text
  | Grass -> green_text
  | Normal -> gray_text

let display_and_choose_move (pokemon : mon) : move =
  (* Display the names of each available move *)
  print_newline ();
  List.iteri
    (fun i move ->
      let colored_move_name = color_of_move_type move.mtype move.move_name in
      Printf.printf "%d: %s\n" (i + 1) colored_move_name)
    pokemon.moveset;

  (* Function to get player's choice and validate it *)
  let rec get_choice () =
    print_string (yellow_text "Enter your choice: ");
    match read_int_opt () with
    | Some choice when choice > 0 && choice <= List.length pokemon.moveset ->
        List.nth pokemon.moveset (choice - 1)
    | _ ->
        print_endline (red_text "Invalid choice, please try again.");
        get_choice ()
  in

  get_choice ()

let leech_seed_effect attacker =
  let sap_amount = 10 in
  (* Amount of HP to gain *)
  attacker.hp <- attacker.hp + sap_amount;
  print_endline
    (attacker.name ^ " gains " ^ string_of_int sap_amount
   ^ " HP from Leech Seed!");
  print_endline
    (attacker.name ^ " is now at " ^ string_of_int attacker.hp ^ " HP\n");
  attacker

let poison_powder_effect attacker defender =
  let poison_damage = 4 in

  defender.is_poisoned <- true;
  defender.hp <- max 0 (defender.hp - poison_damage);

  (* Decrease defender's HP *)
  print_endline
    (defender.name ^ " loses "
    ^ string_of_int poison_damage
    ^ " HP due to Poison Powder!");
  (attacker, defender)

(*Calculate the damage by effectiveness, if less effective/more effective*)
let calculate_damage attacker defender move =
  let type_effect = compare_type attacker defender in
  let base_damage =
    match move.base_power with
    | Some power -> power
    | None -> 0
  in
  base_damage + (type_effect * 10)

let execute_move attacker defender move =
  let hit_chance = if move.accuracy = 100 then 999 else move.accuracy * 10 in

  if Random.int 1000 < hit_chance then (
    print_endline (attacker.name ^ " uses " ^ move.move_name ^ "!");
    let updated_attacker, updated_defender, damage =
      match move.move_name with
      | "leech seed" ->
          let updated_atk = leech_seed_effect attacker in
          (updated_atk, defender, 0)
      | "poison powder" ->
          let updated_atk, updated_def =
            poison_powder_effect attacker defender
          in
          (updated_atk, defender, 0)
      | _ ->
          let dmg = calculate_damage attacker defender move in
          defender.hp <- max 0 (defender.hp - dmg);
          (attacker, defender, dmg)
    in
    (match move.move_name with
    | "leech seed" | "poison powder" -> ()
    | _ ->
        print_endline
          (updated_defender.name ^ " takes " ^ string_of_int damage ^ " damage!"));
    print_endline
      (updated_defender.name ^ " is now at "
      ^ string_of_int updated_defender.hp
      ^ " HP\n");
    updated_defender)
  else begin
    print_endline (attacker.name ^ "'s " ^ move.move_name ^ " missed!");
    defender
  end

let apply_poison_effect pokemon =
  if pokemon.is_poisoned then (
    let poison_damage = 4 in
    pokemon.hp <- max 0 (pokemon.hp - poison_damage);
    print_endline
      (pokemon.name ^ " loses "
      ^ string_of_int poison_damage
      ^ " HP due to Poison!"))

(* Simple check to see if fainted or not*)
let team_has_fainted team =
  match team with
  | [] -> false
  | _ -> List.for_all (fun pokemon -> pokemon.hp <= 0) team

let remove_fainted_pokemon team =
  List.filter (fun pokemon -> pokemon.hp >= 0) team

let choose_next_pokemon player_name team =
  Printf.printf "%s, choose your next Pokemon:\n" player_name;

  (* Display the Pokémon in the team with their indexes *)
  List.iteri
    (fun i pokemon ->
      let colored_pokemon_name = color_of_ptype pokemon.ptype pokemon.name in
      if pokemon.hp > 0 then
        Printf.printf "%d: %s\n" (i + 1) colored_pokemon_name)
    team;

  print_endline "\n";

  (* Function to get the player's choice *)
  let rec get_choice () =
    print_string (yellow_text "Enter your choice: ");
    match read_int_opt () with
    | Some choice when choice > 0 && choice <= List.length team ->
        let chosen_pokemon = List.nth team (choice - 1) in
        if chosen_pokemon.hp > 0 then chosen_pokemon
        else begin
          print_endline (red_text "You cannot choose a fainted Pokémon.");
          get_choice ()
        end
    | _ ->
        print_endline (red_text "Invalid choice, please try again.");
        get_choice ()
  in
  get_choice ()

let rec player_turn player_name current_pokemon player_team opponent_pokemon
    opponent_team =
  if current_pokemon.hp <= 0 then begin
    print_endline
      (player_name ^ " , " ^ current_pokemon.name
     ^ " has fainted and cannot move!");
    opponent_pokemon
  end
  else begin
    print_endline
      (purple_text
         (player_name ^ ", choose an action for " ^ current_pokemon.name
        ^ " against " ^ opponent_pokemon.name));
    print_endline "1: Choose a move";
    print_endline "2: Switch Pokemon \n";
    match read_int_opt () with
    | Some 1 ->
        let chosen_move = display_and_choose_move current_pokemon in
        execute_move current_pokemon opponent_pokemon chosen_move
    | Some 2 ->
        let next_pokemon = choose_next_pokemon player_name player_team in
        print_endline (player_name ^ " switches to " ^ next_pokemon.name);
        next_pokemon
    | _ ->
        print_endline (red_text "Invalid choice, please try again.");
        player_turn player_name current_pokemon player_team opponent_pokemon
          opponent_team
  end

let rec battle_turn player1_name player1_pokemon player1_team player2_name
    player2_pokemon player2_team =
  (* Remove fainted Pokémon from each team *)
  let updated_player1_team = remove_fainted_pokemon player1_team in
  let updated_player2_team = remove_fainted_pokemon player2_team in

  (* Check if either team has completely fainted, ending the battle *)
  if team_has_fainted updated_player1_team then begin
    print_endline (yellow_text (player2_name ^ " wins the battle!"));
    None (* Player 2 wins *)
  end
  else if team_has_fainted updated_player2_team then begin
    print_endline (yellow_text (player1_name ^ " wins the battle!"));
    None (* Player 1 wins *)
  end
  else begin
    (* Determine the Pokémon for the next turn *)
    let next_player1_pokemon, next_player2_pokemon =
      if player1_pokemon.hp <= 0 then
        (* Player 1's Pokémon has fainted; choose the next available Pokémon *)
        let chosen_pokemon =
          choose_next_pokemon player1_name updated_player1_team
        in
        (chosen_pokemon, player2_pokemon)
      else if player2_pokemon.hp <= 0 then
        (* Player 2's Pokémon has fainted; choose the next available Pokémon *)
        let chosen_pokemon =
          choose_next_pokemon player2_name updated_player2_team
        in
        (player1_pokemon, chosen_pokemon)
      else
        (* No Pokémon has fainted; continue with the current Pokémon *)
        (player1_pokemon, player2_pokemon)
    in

    (* Execute each player's turn and get the updated Pokémon *)
    let updated_player2_pokemon =
      List.iter apply_poison_effect player2_team;
      player_turn player1_name next_player1_pokemon updated_player1_team
        next_player2_pokemon updated_player2_team
    in

    let updated_player1_pokemon =
      List.iter apply_poison_effect player1_team;
      player_turn player2_name updated_player2_pokemon updated_player2_team
        next_player1_pokemon updated_player1_team
    in

    (* Recursively call battle_turn with the updated Pokémon and teams *)
    battle_turn player1_name updated_player1_pokemon updated_player1_team
      player2_name updated_player2_pokemon updated_player2_team
  end

let start_battle player1_name player1_team player2_name player2_team =
  (* Starts the battle, each person picks their initial pokemon *)
  let initial_pokemon1 = choose_next_pokemon player1_name player1_team in
  let initial_pokemon2 = choose_next_pokemon player2_name player2_team in
  battle_turn player1_name initial_pokemon1 player1_team player2_name
    initial_pokemon2 player2_team
