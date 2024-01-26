open Game
open Pokemon
open Battle
open Start

let yellow_text text = "\027[33m" ^ text ^ "\027[0m"
let red_text text = "\027[31m" ^ text ^ "\027[0m"
let green_text text = "\027[32m" ^ text ^ "\027[0m"
let blue_text text = "\027[34m" ^ text ^ "\027[0m"
let gray_text text = "\027[90m" ^ text ^ "\027[0m"
let purple_text text = "\027[35m" ^ text ^ "\027[0m"

let color_of_ptype ptype =
  match ptype with
  | Fire -> red_text
  | Water -> blue_text
  | Grass -> green_text
  | Normal -> gray_text

let rec main_loop () =
  print_string "Enter a command (!help or !pokedex or !start or !moves) : ";
  match read_line () with
  | "!help" ->
      print_endline "\nWelcome to the Help Section of Pokemon Showdown!";
      print_endline "Commands available:";
      print_endline
        "!start - Begin a new game. Each player will choose their starter \
         Pokémon and engage in battle.";
      print_endline "!help - Display this help message.";
      print_endline "\nHow to Play:";
      print_endline "1. Start the game by entering the '!start' command.";
      print_endline "2. Each player will be prompted to enter their name.";
      print_endline "3. Choose your starter Pokémon from the available list.";
      print_endline
        "4. Once both players have chosen their Pokémon, the battle will begin.";
      print_endline
        "5. During battle, you can choose to either attack with one of your \
         Pokémon's moves or switch Pokémon.";
      print_endline
        "6. The battle continues until all of one player's Pokémon have \
         fainted.";
      print_endline "\nTips:";
      print_endline
        "- Pay attention to Pokémon types! Some types are more effective \
         against others.";
      print_endline
        "- Use strategy when choosing whether to attack or switch Pokémon.";
      print_endline
        "- Have fun and experiment with different Pokémon and move \
         combinations.";
      print_endline "\nGood luck in your battles!";
      main_loop ()
  | "!pokedex" ->
      print_endline "\nAvailable Pokémon:";
      List.iter display_pokemon_details all_pokemon;
      print_newline ();
      main_loop ()
  | "!start" -> (
      let player1_name = get_player_name "Player 1" in
      let player1_team = choose_starters player1_name in
      let player2_name = get_player_name "Player 2" in
      let player2_team = choose_starters player2_name in

      display_team player1_name player1_team;
      display_team player2_name player2_team;

      match
        start_battle player1_name player1_team player2_name player2_team
      with
      | None -> print_endline "Battle concluded."
      | Some winner -> Printf.printf "%s wins the battle!\n" winner.name)
  | "!moves" ->
      print_endline "\nSpecial Moves:";
      print_endline "Most moves are physical, but some are special.";
      print_endline
        "Leech Seed is one example of a special move, it will regen hp every \
         round, with no max HP..";

      print_endline
        "Poison Powder is another example of a special move, it will attack \
         the ";
      print_endline
        "opponent's HP directly and poison the opponent, taking hp from the \
         opponent every round.";
      print_newline ();
      main_loop ()
  | _ ->
      print_endline "Invalid command. Please try another command.";
      main_loop ()

let () =
  print_endline (blue_text "                                  ,'\\");
  print_endline
    (blue_text "    _.----.        ____         ,'  _\\   ___    ___     ____");
  print_endline
    (blue_text
       "_,-'       `.     |    |  /`.   \\,-'    |   \\  /   |   |    \\  |`\\"
    ^ " ");
  print_endline
    (blue_text
       "\\      __    \\    '-.  | /   `.  ___    |    \\/    |   '-.   \\ |  |");
  print_endline
    (blue_text
       " \\.    \\ \\   |  __  |  |/    ,','_  `.  |          | __  |    \\|  |");
  print_endline
    (blue_text
       "   \\    \\/   /,' _`.|      ,' / / / /   |          ,' _`.|     |  |");
  print_endline
    (blue_text
       "    \\     ,-'/  /   \\    ,'   | \\/ / ,`.|         /  /   \\  |     |");
  print_endline
    (blue_text
       "     \\    \\ |   \\_/  |   `-.  \\    `'  /|  |    ||   \\_/  | \
        |\\    |");
  print_endline
    (blue_text
       "      \\    \\ \\      /       `-.`.___,-' |  |\\  /| \\      /  | |   \
        |");
  print_endline
    (blue_text
       "       \\    \\ `.__,'|  |`-._    `|      |__| \\/ |  `.__,'|  | |   |");
  print_endline
    (blue_text
       "        \\_.-'       |__|    `-._ |              '-.|     '-.| |   |");
  print_endline
    (blue_text
       "                                `'                            '-._|");
  print_endline (yellow_text "                Welcome to the Pokemon Showdown!");
  main_loop ()
