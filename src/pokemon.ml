open Yojson.Basic.Util

type ptype =
  | Fire
  | Water
  | Grass
  | Normal

type move = {
  move_name : string;
  base_power : int option;
  mtype : ptype;
  effects : string;
  accuracy : int;
}

let get_movename move = move.move_name
let get_basepower move = move.base_power
let get_mtype move = move.mtype
let get_effects move = move.effects
let get_accuracy move = move.accuracy

type mon = {
  name : string;
  mutable hp : int;
  ptype : ptype;
  moveset : move list;
  mutable is_poisoned : bool;
}

let get_hp p = p.hp
let get_name p = p.name
let get_ptype p = p.ptype
let get_moveset p = p.moveset

let compare_type p1 p2 =
  match (p1.ptype, p2.ptype) with
  | Fire, Water -> -1
  | Water, Fire -> 1
  | Fire, Grass -> 1
  | Grass, Fire -> -1
  | Grass, Water -> 1
  | Water, Grass -> -1
  | Water, Water -> 0
  | Fire, Fire -> 0
  | Grass, Grass -> 0
  | Normal, Normal -> 0
  | (Fire | Water | Grass), Normal -> 0
  | Normal, (Fire | Water | Grass) -> 0

let compare_hp p1 p2 = compare p1.hp p2.hp

let find_move_by_name all_moves name =
  try List.find (fun m -> m.move_name = name) all_moves
  with Not_found -> failwith ("Move not found: " ^ name)

let parse_ptype ptype_string =
  match ptype_string with
  | "Fire" -> Fire
  | "Water" -> Water
  | "Grass" -> Grass
  | "Normal" -> Normal
  | _ -> failwith ("Unknown ptype: " ^ ptype_string)

let parse_move json =
  {
    move_name = json |> member "move_name" |> to_string;
    base_power =
      (try Some (json |> member "base_power" |> to_int)
       with Type_error _ -> None);
    (* Correctly handle null values *)
    mtype = json |> member "mtype" |> to_string |> parse_ptype;
    effects = json |> member "effects" |> to_string;
    accuracy = json |> member "accuracy" |> to_int;
  }

(* All moves data from the JSON file *)
let parse_move_list file =
  let json = Yojson.Basic.from_file file in
  let moves_list = json |> member "moveset" |> to_list in
  (* Updated line *)
  List.map parse_move moves_list

(*Parse single pokemon from Json File*)
let parse_pokemon all_moves json =
  let name = json |> member "name" |> to_string in
  let hp = json |> member "hp" |> to_int in
  let ptype = json |> member "ptype" |> to_string |> parse_ptype in
  let moveset =
    json |> member "moveset" |> to_list |> List.map to_string
    |> List.map (find_move_by_name all_moves)
  in

  { name; hp; ptype; moveset; is_poisoned = false }

let parse_pokemon_list all_moves file =
  let json = Yojson.Basic.from_file file in
  let pokemon_list = json |> member "pokemon" |> to_list in
  List.map (parse_pokemon all_moves) pokemon_list

let all_moves = parse_move_list "data/moves.json"
let all_pokemon = parse_pokemon_list all_moves "data/pokemon.json"
