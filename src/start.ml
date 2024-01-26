open Pokemon
open Battle

let red_text text = "\027[31m" ^ text ^ "\027[0m"
let green_text text = "\027[32m" ^ text ^ "\027[0m"
let blue_text text = "\027[34m" ^ text ^ "\027[0m"
let gray_text text = "\027[90m" ^ text ^ "\027[0m"
let purple_text text = "\027[35m" ^ text ^ "\027[0m"
let yellow_text text = "\027[33m" ^ text ^ "\027[0m"

let color_of_ptype ptype =
  match ptype with
  | Fire -> red_text
  | Water -> blue_text
  | Grass -> green_text
  | Normal -> gray_text

let rec get_player_name player_label =
  print_string
    (purple_text player_label ^ purple_text ", please enter your name: ");
  let name = read_line () in

  try
    let _ = int_of_string name in
    print_endline
      (red_text
         "Invalid choice, please enter a name that does not consist of only \
          numbers.");
    get_player_name player_label
  with Failure _ ->
    if name <> "" then name
    else begin
      print_endline (red_text "Invalid input, please enter a number.");
      get_player_name player_label
    end

let string_of_ptype = function
  | Fire -> "Fire"
  | Water -> "Water"
  | Grass -> "Grass"
  | Normal -> "Normal"

let display_pokemon_details pokemon =
  let colored_name = color_of_ptype pokemon.ptype pokemon.name in
  Printf.printf "Name: %s\n" colored_name;
  Printf.printf "Type: %s\n" (string_of_ptype pokemon.ptype);
  Printf.printf "HP: %d\n" pokemon.hp;
  Printf.printf "Moves:\n";
  List.iter
    (fun move ->
      Printf.printf "- %s (Power: %s, Accuracy: %d%%)\n" move.move_name
        (match move.base_power with
        | Some bp -> string_of_int bp
        | None -> "N/A")
        move.accuracy)
    pokemon.moveset;
  print_newline ()

let display_available_pokemon () =
  print_endline "Available Starters:";
  List.iteri
    (fun i p ->
      let colored_name = color_of_ptype p.ptype p.name in
      Printf.printf "%d: %s\n" (i + 1) colored_name)
    all_pokemon;
  print_newline ()

let rec choose_starters player_name =
  print_endline (player_name ^ ", choose your starter Pokemon:");
  display_available_pokemon ();

  let rec choose_helper n acc =
    if n = 0 then acc
    else begin
      print_string (yellow_text "Enter the number of your choice: ");
      try
        let choice = read_int () in
        if choice > 0 && choice <= List.length all_pokemon then
          let selected_pokemon = List.nth all_pokemon (choice - 1) in
          if not (List.exists (fun p -> p.name = selected_pokemon.name) acc)
          then choose_helper (n - 1) (acc @ [ selected_pokemon ])
          else begin
            print_endline
              (red_text
                 "You have already chosen this Pokémon. Please choose a \
                  different one.");
            choose_helper n acc
          end
        else begin
          print_endline
            (red_text
               "Invalid choice, please choose a number within the available \
                range.");
          choose_helper n acc
        end
      with Failure _ ->
        print_endline (red_text "Invalid input, please enter a number.");
        choose_helper n acc
    end
  in
  let chosen_pokemon = choose_helper 3 [] in
  print_endline "\n";
  chosen_pokemon

let rec display_team player_name pokemon_list =
  print_endline (player_name ^ "'s Team:");
  List.iter
    (fun pokemon ->
      let colored_name = color_of_ptype pokemon.ptype pokemon.name in
      print_endline colored_name)
    pokemon_list
