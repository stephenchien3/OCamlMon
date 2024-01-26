open OUnit2
open Game
open Pokemon
open Format
open Battle
open Printf
open Start

(** ------------------------------ Test Plan ----------------------------------

    A set of 6 basic moves, 4 basic pokemon and 3 sample teams are provided in
    this test file for use by the following testing functions.

    The mechanics of each test are written within separate functions above the
    bodies of the test suites. Each test function directly exercises one piece
    of functionality within a module, which could be a single function, or a
    combination of various sized functions that are closely coupled. These test
    functions are given semi-descriptive names, and are further described by the
    string preceding each in the test suite.

    The execution of the tests are divided up into 7 suites. Each suite focuses
    on a specific type of testing. For example, tests that test user input or
    program output to stdin and stdout are separated from tests for the rest of
    the functions.

    - Pokemon compilation unit: the entirety of the pokemon module is unit
      tested.
    - Battle compilation unit: most functions described in the signature of the
      battle module are unit tested. This does not include helper functions
      excluded from the signature. However, functions like battle_turn and
      start_battle were not unit tested due to their highly variable results in
      the events of different teams and non-default hp levels. These functions
      were thorougly tested in our player interface in the terminal.
    - Start compilation unit: most functions described in the signature of the
      start module are unit tested. This does not include helper functions
      excluded from the signature.

    Tests were developed systematially, first with a focus on purely internal
    aspects of the game, i.e. functions that don't require direct user input and
    don't print anything to stdout. To go about testing the rest of the
    functions, we used an approach reliant on redirecting user input away from
    the terminal and instead to sample input txt files. This way we were able to
    simulate potential behavior of a player by providing valid and invalid
    input, and comparing the output of our functions to what we expect from
    those inputs. When these functions included side effects of printing prompts
    to stdout, we redirected the output to null to prevent the terminal from
    being flooded with text during a run of the test suite. For functions meant
    only to print information, we directed the output to a text file and used a
    custom file equality assertion function to verify printing was correct.

    For the most part, we used black-box testing to ensure the correctness of
    our game system. This was necessary and sufficient for functions without
    side effects. We used glass-box testing for the other functions with I/O
    interaction, so we could be sure our tests exercised all possible cases that
    wouldn't be known as possibilities to a tester otherwise, such as different
    invalid input messages built into the function.

    We believe our test suites provide execellent coverage for the features of
    this game. We used rigorous testing practices throughout, ensuring we hit
    all possible cases that don't violate preconditions, including a series of
    edge case tests modeling typical and atypical gameplay. The test functions
    exercise all internal and most I/O functions in our system, and are
    organized efficiently so that changes to functions can be easily accomodated
    in an updated test suite. *)

(* Basic Moves for Testing *)
let flamethrower =
  {
    move_name = "Flamethrower";
    base_power = Some 90;
    mtype = Fire;
    effects = "Burns the target.";
    accuracy = 100;
  }

let water_gun =
  {
    move_name = "Water Gun";
    base_power = Some 40;
    mtype = Water;
    effects = "No additional effect.";
    accuracy = 100;
  }

let vine_whip =
  {
    move_name = "Vine Whip";
    base_power = Some 45;
    mtype = Grass;
    effects = "No additional effect.";
    accuracy = 100;
  }

let tackle =
  {
    move_name = "Tackle";
    base_power = Some 40;
    mtype = Normal;
    effects = "No additional effect.";
    accuracy = 100;
  }

let move_with_no_base_power =
  {
    move_name = "Growl";
    base_power = None;
    mtype = Normal;
    effects = "Lowers the target's attack.";
    accuracy = 100;
  }

let move_with_high_base_power =
  {
    move_name = "Growl";
    base_power = Some 150;
    mtype = Normal;
    effects = "Lowers the target's attack.";
    accuracy = 100;
  }

(* Basic Pokemon for Testing *)
let charmander =
  {
    name = "Charmander";
    hp = 50;
    ptype = Fire;
    moveset = [ flamethrower; tackle ];
    is_poisoned = false (* Assuming Charmander knows Flamethrower and Tackle *);
  }

let squirtle =
  {
    name = "Squirtle";
    hp = 50;
    ptype = Water;
    moveset = [ water_gun; tackle ];
    is_poisoned = false (* Assuming Squirtle knows Water Gun and Tackle *);
  }

let bulbasaur =
  {
    name = "Bulbasaur";
    hp = 50;
    ptype = Grass;
    moveset = [ vine_whip; tackle ];
    is_poisoned = false;
    (* Assuming Bulbasaur knows Vine Whip and Tackle *)
  }

let rattata =
  {
    name = "Rattata";
    hp = 30;
    ptype = Normal;
    moveset = [ tackle; move_with_no_base_power ];
    is_poisoned = false;
    (* Assuming Rattata knows Tackle and Growl *)
  }

let team1 = [ charmander; squirtle; rattata ]
let team2 = [ { bulbasaur with hp = 0 }; charmander; { rattata with hp = 0 } ]
let team3 = [ { bulbasaur with hp = 0 }; charmander; bulbasaur ]

let test_pokemon_comparison_negative_hp _ =
  let charmander =
    {
      name = "Charmander";
      hp = -5;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal 1 (compare_type squirtle charmander)

let test_remove_fainted_pokemon _ =
  let mixed_hp_team = [ charmander; { squirtle with hp = -5 }; rattata ] in
  let clean_team = remove_fainted_pokemon mixed_hp_team in
  assert_equal [ charmander; rattata ] clean_team

let test_team_fainting_detection_all_fainted _ =
  let fainted_team =
    [
      { charmander with hp = 0 };
      { squirtle with hp = 0 };
      { rattata with hp = 0 };
    ]
  in
  assert (team_has_fainted fainted_team)

let test_remove_fainted_pokemon_empty_team _ =
  let empty_team = [] in
  assert_equal [] (remove_fainted_pokemon empty_team)

let test_pokemon_comparison_same_hp_different_type _ =
  let charmander =
    {
      name = "Charmander";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  let squirtle =
    {
      name = "Squirtle";
      hp = 50;
      ptype = Water;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal 0 (compare_hp charmander squirtle)

let test_pokemon_comparison_higher_hp_different_type _ =
  let charmander =
    {
      name = "Charmander";
      hp = 60;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  let squirtle =
    {
      name = "Squirtle";
      hp = 50;
      ptype = Water;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal 1 (compare_hp charmander squirtle)

let test_pokemon_comparison_lower_hp_different_type _ =
  let charmander =
    {
      name = "Charmander";
      hp = 40;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  let squirtle =
    {
      name = "Squirtle";
      hp = 50;
      ptype = Water;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal (-1) (compare_hp charmander squirtle)

let test_pokemon_comparison_higher_hp_same_type _ =
  let charmander =
    {
      name = "Charmander";
      hp = 60;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  let charmeleon =
    {
      name = "Charmeleon";
      hp = 70;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal (-1) (compare_hp charmander charmeleon)

let test_pokemon_comparison_same_hp_same_type _ =
  let charmander =
    {
      name = "Charmander";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  let charmeleon =
    {
      name = "Charmeleon";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal 0 (compare_hp charmander charmeleon)

let test_pokemon_comparison_lower_hp_same_type _ =
  let charmander =
    {
      name = "Charmander";
      hp = 40;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  let charmeleon =
    {
      name = "Charmeleon";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal (-1) (compare_hp charmander charmeleon)

let test_pokemon_comparison_same_type_different_hp _ =
  let charmander1 =
    {
      name = "Charmander1";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  let charmander2 =
    {
      name = "Charmander2";
      hp = 60;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal (-1) (compare_hp charmander1 charmander2)

let test_pokemon_comparison_different_type_different_hp _ =
  let bulbasaur =
    {
      name = "Bulbasaur";
      hp = 45;
      ptype = Grass;
      moveset = [];
      is_poisoned = false;
    }
  in
  let charmander =
    {
      name = "Charmander";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal (-1) (compare_hp bulbasaur charmander)

let test_pokemon_comparison_same_hp_different_type _ =
  let bulbasaur =
    {
      name = "Bulbasaur";
      hp = 50;
      ptype = Grass;
      moveset = [];
      is_poisoned = false;
    }
  in
  let charmander =
    {
      name = "Charmander";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal (-1) (compare_type bulbasaur charmander)

let test_pokemon_comparison_different_hp_different_type _ =
  let bulbasaur =
    {
      name = "Bulbasaur";
      hp = 45;
      ptype = Grass;
      moveset = [];
      is_poisoned = false;
    }
  in
  let charmander =
    {
      name = "Charmander";
      hp = 50;
      ptype = Fire;
      moveset = [];
      is_poisoned = false;
    }
  in
  assert_equal (-1) (compare_hp bulbasaur charmander)

let test_parse_ptype _ =
  assert_equal Fire (parse_ptype "Fire");
  assert_equal Water (parse_ptype "Water");
  assert_equal Grass (parse_ptype "Grass");
  assert_equal Normal (parse_ptype "Normal");
  assert_raises (Failure "Unknown ptype: Electric") (fun () ->
      parse_ptype "Electric");
  assert_raises (Failure "Unknown ptype: XYZ") (fun () -> parse_ptype "XYZ")

let test_parse_move _ =
  let move_json =
    `Assoc
      [
        ("move_name", `String "flamethrower");
        ("base_power", `Int 90);
        ("mtype", `String "Fire");
        ("effects", `String "Burns the target.");
        ("accuracy", `Int 100);
      ]
  in
  let parsed_move = parse_move move_json in
  assert_equal "flamethrower" parsed_move.move_name;
  assert_equal (get_movename parsed_move) parsed_move.move_name;
  assert_equal (Some 90) parsed_move.base_power;
  assert_equal (get_basepower parsed_move) parsed_move.base_power;
  assert_equal Fire parsed_move.mtype;
  assert_equal (get_mtype parsed_move) parsed_move.mtype;
  assert_equal "Burns the target." parsed_move.effects;
  assert_equal (get_effects parsed_move) parsed_move.effects;
  assert_equal 100 parsed_move.accuracy;
  assert_equal (get_accuracy parsed_move) parsed_move.accuracy

let test_compare_type _ =
  assert_equal (-1)
    (compare_type
       {
         name = "Charmander";
         hp = 50;
         ptype = Fire;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Squirtle";
         hp = 50;
         ptype = Water;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal (-1)
    (compare_type
       {
         name = "Bulbasaur";
         hp = 50;
         ptype = Grass;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Charmander";
         hp = 50;
         ptype = Fire;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal 0
    (compare_type
       {
         name = "Pidgey";
         hp = 50;
         ptype = Normal;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Rattata";
         hp = 50;
         ptype = Normal;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal 1
    (compare_type
       {
         name = "Squirtle";
         hp = 50;
         ptype = Water;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Charmander";
         hp = 50;
         ptype = Fire;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal 1
    (compare_type
       {
         name = "Bulbasaur";
         hp = 50;
         ptype = Grass;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Squirtle";
         hp = 50;
         ptype = Water;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal 1
    (compare_type
       {
         name = "Charmander";
         hp = 50;
         ptype = Fire;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Bulbasaur";
         hp = 50;
         ptype = Grass;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal 0
    (compare_type
       {
         name = "Charmander";
         hp = 50;
         ptype = Fire;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Rattata";
         hp = 50;
         ptype = Normal;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal 0
    (compare_type
       {
         name = "Squirtle";
         hp = 50;
         ptype = Water;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Rattata";
         hp = 50;
         ptype = Normal;
         moveset = [];
         is_poisoned = false;
       });
  assert_equal 0
    (compare_type
       {
         name = "Bulbasaur";
         hp = 50;
         ptype = Grass;
         moveset = [];
         is_poisoned = false;
       }
       {
         name = "Rattata";
         hp = 50;
         ptype = Normal;
         moveset = [];
         is_poisoned = false;
       })

let test_parse_pokemon _ =
  let pokemon_json =
    `Assoc
      [
        ("name", `String "Pikachu");
        ("hp", `Int 35);
        ("ptype", `String "Normal");
        ("moveset", `List [ `String "scratch"; `String "fire fang" ]);
      ]
  in
  let parsed_pokemon = parse_pokemon all_moves pokemon_json in
  assert_equal "Pikachu" parsed_pokemon.name;
  assert_equal 35 parsed_pokemon.hp;
  assert_equal Normal parsed_pokemon.ptype;
  assert_equal 2 (List.length parsed_pokemon.moveset);
  assert_equal "Pikachu" (get_name parsed_pokemon);
  assert_equal 35 (get_hp parsed_pokemon);
  assert_equal Normal (get_ptype parsed_pokemon);
  assert_equal 2 (List.length (get_moveset parsed_pokemon))

let test_parse_move_edge_cases _ =
  let move_json_no_power =
    `Assoc
      [
        ("move_name", `String "Tackle");
        ("base_power", `Null);
        ("mtype", `String "Normal");
        ("effects", `String "No additional effect.");
        ("accuracy", `Int 100);
      ]
  in
  let parsed_move_no_power = parse_move move_json_no_power in
  assert_equal "Tackle" parsed_move_no_power.move_name;
  assert_equal None parsed_move_no_power.base_power;
  assert_equal Normal parsed_move_no_power.mtype;
  assert_equal "No additional effect." parsed_move_no_power.effects;
  assert_equal 100 parsed_move_no_power.accuracy

let test_parse_pokemon_list _ =
  let pokemon_list = parse_pokemon_list all_moves "data/pokemon.json" in

  (* Check the number of parsed Pokémon *)
  assert_equal 6 (List.length pokemon_list);

  (* Check details of the first Pokémon *)
  let charmander = List.hd pokemon_list in
  assert_equal "Charmander" (get_name charmander);
  assert_equal 78 (get_hp charmander);
  assert_equal Fire (get_ptype charmander);
  assert_equal 4 (List.length (get_moveset charmander));

  let bulbasaur = List.nth pokemon_list 1 in
  assert_equal "Bulbasaur" (get_name bulbasaur);
  assert_equal 76 (get_hp bulbasaur);
  assert_equal Grass (get_ptype bulbasaur);
  assert_equal 4 (List.length (get_moveset bulbasaur));

  let cyndaquil = List.hd (List.rev pokemon_list) in
  assert_equal "Cyndaquil" (get_name cyndaquil);
  assert_equal 79 (get_hp cyndaquil);
  assert_equal Fire (get_ptype cyndaquil);
  assert_equal 4 (List.length (get_moveset cyndaquil))

let test_parse_move_list _ =
  let move_list = parse_move_list "data/moves.json" in

  (* Check the number of parsed moves *)
  assert_equal 11 (List.length move_list);

  (* Check details of the first move: Scratch *)
  let scratch = List.hd move_list in
  assert_equal "scratch" scratch.move_name;
  assert_equal (Some 40) scratch.base_power;
  assert_equal Normal scratch.mtype;
  assert_equal "No additional effect." scratch.effects;
  assert_equal 100 scratch.accuracy;

  assert_equal "scratch" (get_movename scratch);
  assert_equal (Some 40) (get_basepower scratch);
  assert_equal Normal (get_mtype scratch);
  assert_equal "No additional effect." (get_effects scratch);
  assert_equal 100 (get_accuracy scratch);

  let leech_seed = List.nth move_list 5 in
  assert_equal "leech seed" leech_seed.move_name;
  assert_equal None leech_seed.base_power;
  assert_equal Grass leech_seed.mtype;
  assert_equal "Saps health from the opponent over several turns."
    leech_seed.effects;
  assert_equal 90 leech_seed.accuracy;
  assert_equal "leech seed" (get_movename leech_seed);
  assert_equal None (get_basepower leech_seed);
  assert_equal Grass (get_mtype leech_seed);
  assert_equal "Saps health from the opponent over several turns."
    (get_effects leech_seed);
  assert_equal 90 (get_accuracy leech_seed);

  let aqua_tail = List.hd (List.rev move_list) in
  assert_equal "aqua tail" aqua_tail.move_name;
  assert_equal (Some 90) aqua_tail.base_power;
  assert_equal Water aqua_tail.mtype;
  assert_equal "No additional effect." aqua_tail.effects;
  assert_equal 90 aqua_tail.accuracy;
  assert_equal (Some 90) (get_basepower aqua_tail);
  assert_equal Water (get_mtype aqua_tail);
  assert_equal "No additional effect." (get_effects aqua_tail);
  assert_equal 90 (get_accuracy aqua_tail)

let test_calculate_damage _ =
  let expected_damage_fire_grass = 90 + (1 * 10) in
  assert_equal expected_damage_fire_grass
    (calculate_damage charmander bulbasaur flamethrower);

  let expected_damage_water_fire = 40 + (1 * 10) in
  assert_equal expected_damage_water_fire
    (calculate_damage squirtle charmander water_gun);

  let expected_damage_grass_water = 45 + (1 * 10) in
  assert_equal expected_damage_grass_water
    (calculate_damage bulbasaur squirtle vine_whip);

  let expected_damage_normal_normal = 40 + (0 * 10) in
  assert_equal expected_damage_normal_normal
    (calculate_damage rattata bulbasaur tackle);

  let expected_damage_no_base_power = 0 + (0 * 10) in
  assert_equal expected_damage_no_base_power
    (calculate_damage rattata charmander move_with_no_base_power);

  let expected_damage_high_base_power = 150 + (0 * 10) in
  assert_equal expected_damage_high_base_power
    (calculate_damage rattata charmander move_with_high_base_power)

let test_execute_move_success _ =
  (*Original Standard output *)
  let orig_stdout = Unix.dup Unix.stdout in
  (*/dev/null (a special file that discards all data written to it) for
    writing *)
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  (*any output that would go to the console will instead go to /dev/null*)
  let attacker = charmander in
  let defender = { bulbasaur with hp = 120 } in
  let move = flamethrower in
  let expected_defender_hp = defender.hp - 100 in

  let defender_after_move = execute_move attacker defender move in

  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;

  assert_equal expected_defender_hp defender_after_move.hp

let test_execute_move_failure _ =
  let orig_stdout = Unix.dup Unix.stdout in

  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;

  let attacker = charmander in
  let defender = { bulbasaur with hp = 120 } in
  let move = { flamethrower with accuracy = 0 } in
  let defender_after_move = execute_move attacker defender move in

  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;

  assert_equal defender.hp
    defender_after_move.hp (* HP should remain the same *)

let test_execute_move_lethal_damage _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;

  let attacker = { charmander with moveset = [ move_with_high_base_power ] } in
  let defender = { rattata with hp = 10 } in

  let defender_after_move =
    execute_move attacker defender move_with_high_base_power
  in

  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;

  assert_equal 0 defender_after_move.hp

let test_execute_move_non_damaging _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;

  let attacker = { charmander with moveset = [ move_with_no_base_power ] } in
  let defender = { rattata with hp = 10 } in

  let defender_after_move =
    execute_move attacker defender move_with_no_base_power
  in

  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;

  assert_equal 10 defender_after_move.hp

let test_empty_team_has_fainted _ = assert (not (team_has_fainted []))

let test_team_has_fainted_all_fainted _ =
  let fainted_team = [ { charmander with hp = 0 }; { rattata with hp = 0 } ] in
  assert (team_has_fainted fainted_team)

let test_team_has_fainted_one_fainted _ =
  let fainted_team = [ { charmander with hp = 2 }; { rattata with hp = 0 } ] in
  assert (not (team_has_fainted fainted_team))

let test_team_has_fainted_none_fainted _ =
  let fainted_team = [ { charmander with hp = 2 }; { rattata with hp = 32 } ] in
  assert (not (team_has_fainted fainted_team))

let test_remove_fainted_empty_team _ =
  assert_equal [] (remove_fainted_pokemon [])

let test_remove_all_fainted _ =
  let all_fainted_team =
    [ { charmander with hp = -2 }; { rattata with hp = -2 } ]
  in
  let result = remove_fainted_pokemon all_fainted_team in
  assert_equal [] result

let test_remove_some_fainted _ =
  let mixed_team = [ { charmander with hp = -4 }; { squirtle with hp = 20 } ] in
  let result = remove_fainted_pokemon mixed_team in
  assert_equal [ { squirtle with hp = 20 } ] result

let test_remove_none_fainted _ =
  let no_fainted_team =
    [ { bulbasaur with hp = 30 }; { squirtle with hp = 20 } ]
  in
  let result = remove_fainted_pokemon no_fainted_team in
  assert_equal no_fainted_team result

let test_display_and_choose_move1 _ =
  (*Original Standard output *)
  let orig_stdout = Unix.dup Unix.stdout in
  (*/dev/null (a special file that discards all data written to it) for
    writing *)
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  (*any output that would go to the console will instead go to /dev/null*)
  let in_channel = open_in "tests/samples/choosemove1.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  (* use a file as input instead of stdin *)
  let move = display_and_choose_move charmander in
  Unix.dup2 standard_in Unix.stdin;
  (* replace standard input *)
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal flamethrower move

let test_display_and_choose_move2 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/choosemove2.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let move = display_and_choose_move squirtle in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal water_gun move

let test_display_and_choose_move3 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/choosemove3.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let move = display_and_choose_move rattata in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal move_with_no_base_power move

let test_choose_next1 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/choosemon1.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let new_mon = choose_next_pokemon "--" team1 in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal squirtle new_mon

let test_choose_next2 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/choosemon2.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let new_mon = choose_next_pokemon "--" team1 in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal rattata new_mon

let test_choose_next3 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/choosemon3.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let new_mon = choose_next_pokemon "--" team3 in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal charmander new_mon

let test_choose_next4 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/choosemon4.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let new_mon = choose_next_pokemon "--" team2 in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal charmander new_mon

let test_choose_next5 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/choosemon5.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let new_mon = choose_next_pokemon "--" team3 in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal bulbasaur new_mon

let test_player_name1 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/playername1.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let new_mon = get_player_name "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal "John Doe" new_mon

let test_player_name2 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/playername2.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let name = get_player_name "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal "Jane Doe" name

let test_player_name3 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/playername3.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let name = get_player_name "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal "*@#$!@#$" name

let test_player_name4 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/playername4.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let name = get_player_name "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal "John456!!" name

let test_player_name5 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/playername5.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let name = get_player_name "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal "asdf" name

let test_string_of_ptype _ =
  assert_equal "Fire" (string_of_ptype Fire);
  assert_equal "Water" (string_of_ptype Water);
  assert_equal "Grass" (string_of_ptype Grass);
  assert_equal "Normal" (string_of_ptype Normal)

let test_choose_starters1 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/starters1.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let mons = choose_starters "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal
    [ List.nth all_pokemon 0; List.nth all_pokemon 1; List.nth all_pokemon 2 ]
    mons

let test_choose_starters2 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/starters2.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let mons = choose_starters "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal
    [ List.nth all_pokemon 4; List.nth all_pokemon 1; List.nth all_pokemon 0 ]
    mons

let test_choose_starters3 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/starters3.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let mons = choose_starters "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal
    [ List.nth all_pokemon 2; List.nth all_pokemon 0; List.nth all_pokemon 5 ]
    mons

let test_choose_starters4 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/starters4.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let mons = choose_starters "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal
    [ List.nth all_pokemon 5; List.nth all_pokemon 3; List.nth all_pokemon 4 ]
    mons

let test_choose_starters5 _ =
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/starters5.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let mons = choose_starters "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_equal
    [ List.nth all_pokemon 4; List.nth all_pokemon 5; List.nth all_pokemon 0 ]
    mons

let assert_file_equal file1 file2 =
  let channel1 = open_in file1 in
  let channel2 = open_in file2 in
  try
    let rec compare_lines () =
      match String.compare (input_line channel1) (input_line channel2) with
      | 0 -> compare_lines ()
      | _ -> failwith "files not equal"
    in
    compare_lines ()
  with
  | End_of_file ->
      close_in channel1;
      close_in channel2;
      assert true
  | _ ->
      close_in channel1;
      close_in channel2;
      assert false

let clear_comp_out_file path = Unix.truncate path 0

let test_display_team1 _ =
  let compout = "tests/samples/compout1.txt" in
  clear_comp_out_file compout;
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/starters1.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let mons = choose_starters "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;

  let orig_stdout = Unix.dup Unix.stdout in
  let temp_out = Unix.openfile compout [ Unix.O_WRONLY ] 0 in
  Unix.dup2 temp_out Unix.stdout;
  Unix.close temp_out;
  display_team "Player 1" mons;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_file_equal "tests/samples/dispteam1.txt" compout

let test_display_team2 _ =
  let compout = "tests/samples/compout2.txt" in
  clear_comp_out_file compout;
  let orig_stdout = Unix.dup Unix.stdout in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 dev_null Unix.stdout;
  Unix.close dev_null;
  let in_channel = open_in "tests/samples/starters2.txt" in
  let standard_in = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel in_channel) Unix.stdin;
  let mons = choose_starters "Player 1" in
  Unix.dup2 standard_in Unix.stdin;
  close_in in_channel;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;

  let orig_stdout = Unix.dup Unix.stdout in
  let temp_out = Unix.openfile compout [ Unix.O_WRONLY ] 0 in
  Unix.dup2 temp_out Unix.stdout;
  Unix.close temp_out;
  display_team "User1234" mons;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_file_equal "tests/samples/dispteam2.txt" compout

let test_display_empty_team _ =
  let compout = "tests/samples/compout3.txt" in
  clear_comp_out_file compout;
  let orig_stdout = Unix.dup Unix.stdout in
  let temp_out = Unix.openfile compout [ Unix.O_WRONLY ] 0 in
  Unix.dup2 temp_out Unix.stdout;
  Unix.close temp_out;
  display_team "Player2024" [];
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_file_equal "tests/samples/dispteam3.txt" compout

let test_display_details1 _ =
  let compout = "tests/samples/compout4.txt" in
  clear_comp_out_file compout;
  let orig_stdout = Unix.dup Unix.stdout in
  let temp_out = Unix.openfile compout [ Unix.O_WRONLY ] 0 in
  Unix.dup2 temp_out Unix.stdout;
  Unix.close temp_out;
  display_pokemon_details charmander;
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_file_equal "tests/samples/details1.txt" compout

let test_display_avail_pokemon _ =
  let compout = "tests/samples/compout5.txt" in
  clear_comp_out_file compout;
  let orig_stdout = Unix.dup Unix.stdout in
  let temp_out = Unix.openfile compout [ Unix.O_WRONLY ] 0 in
  Unix.dup2 temp_out Unix.stdout;
  Unix.close temp_out;
  display_available_pokemon ();
  Unix.dup2 orig_stdout Unix.stdout;
  Unix.close orig_stdout;
  assert_file_equal "tests/samples/availpoke.txt" compout

let pokemon_test =
  [
    "parse_ptype" >:: test_parse_ptype;
    "parse_move" >:: test_parse_move;
    "compare_type" >:: test_compare_type;
    "parse_pokemon" >:: test_parse_pokemon;
    "parse_pokemon_list" >:: test_parse_pokemon_list;
    "parse_move_list" >:: test_parse_move_list;
    "parse_move_edge_cases" >:: test_parse_move_edge_cases;
  ]

let edge_tests =
  [
    "comparing negative hp" >:: test_pokemon_comparison_negative_hp;
    "removing fainted pokemon" >:: test_remove_fainted_pokemon;
    "detect when an entire team has fainted"
    >:: test_team_fainting_detection_all_fainted;
    "testing removing fainted pokemon from an empty team"
    >:: test_remove_fainted_pokemon_empty_team;
    "comparing pokemon with same and different hp"
    >:: test_pokemon_comparison_same_hp_different_type;
    "comparing higher hp different type"
    >:: test_pokemon_comparison_higher_hp_different_type;
    "comparing lower hp different type"
    >:: test_pokemon_comparison_lower_hp_different_type;
    "comparing higher hp same type"
    >:: test_pokemon_comparison_higher_hp_same_type;
    "comparing same hp same type" >:: test_pokemon_comparison_same_hp_same_type;
    "comparing lower hp same type"
    >:: test_pokemon_comparison_lower_hp_same_type;
    "comparing different hp same type"
    >:: test_pokemon_comparison_same_type_different_hp;
    "comparing different hp different type"
    >:: test_pokemon_comparison_different_type_different_hp;
    "comparing same hp different type"
    >:: test_pokemon_comparison_same_hp_different_type;
    "comparing different hp different type"
    >:: test_pokemon_comparison_different_hp_different_type;
  ]

let battle_tests =
  [
    "test_damage" >:: test_calculate_damage;
    "test_execute_move_success" >:: test_execute_move_success;
    "test_execute_move_failure" >:: test_execute_move_failure;
    "test_execute_move_lethal" >:: test_execute_move_lethal_damage;
    "test_non_damaging" >:: test_execute_move_non_damaging;
    "all_team_has_fainted" >:: test_team_has_fainted_all_fainted;
    "some_team_has_fainted" >:: test_team_has_fainted_one_fainted;
    "none_team_has_fainted" >:: test_team_has_fainted_none_fainted;
    "empty_team_has_not_fainted" >:: test_empty_team_has_fainted;
    "remove_all_fainted" >:: test_remove_all_fainted;
    "remove_some_fainted" >:: test_remove_some_fainted;
    "remove_none_fainted" >:: test_remove_none_fainted;
    "remove_fainted_from_empty_team" >:: test_remove_fainted_empty_team;
  ]

let start_tests =
  [ "testing string_of_ptype for all types" >:: test_string_of_ptype ]

let battle_user_input_functions_test =
  [
    "choose move from pokemon" >:: test_display_and_choose_move1;
    "choose move from pokemon first choice invalid second valid"
    >:: test_display_and_choose_move2;
    "chose move from pokemon multiple tries till valid choice"
    >:: test_display_and_choose_move3;
    "choose next pokemon valid choice" >:: test_choose_next1;
    "choose next pokemon valid choice on second try" >:: test_choose_next2;
    "choose next pokemon first choice fainted valid choice on second"
    >:: test_choose_next3;
    "choose next pokemon first few choices fainted valid choice on second"
    >:: test_choose_next4;
    "choose next pokemon first few choices invalid including trying 0 index"
    >:: test_choose_next5;
  ]

let start_input_functions_test =
  [
    "player input integer as name before valid name" >:: test_player_name1;
    "testing player input valid name first try" >:: test_player_name2;
    "testing player using only symbols as name" >:: test_player_name3;
    "testing player using combination of character types" >:: test_player_name4;
    "testing blanks entered between valid and invalid inputs"
    >:: test_player_name5;
    "testing consecutive valid inputs" >:: test_choose_starters1;
    "testing out of bounds inputs among valid" >:: test_choose_starters2;
    "testing symbols among valid inputs" >:: test_choose_starters3;
    "testing strings and symbols and mixed among valid inputs"
    >:: test_choose_starters4;
    "testing with a lot of gibberish entered and 3 valid inputs scattered \
     among said gibberish" >:: test_choose_starters5;
  ]

let start_output_functions_test =
  [
    "test display team first 3 pokemon and reading selection"
    >:: test_display_team1;
    "test display team with invalid inputs during enter team stage"
    >:: test_display_team2;
    "test displaying the empty team" >:: test_display_empty_team;
    "test display pokemon details charmander" >:: test_display_details1;
    "test display available pokemon" >:: test_display_avail_pokemon;
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           pokemon_test;
           edge_tests;
           battle_tests;
           start_tests;
           battle_user_input_functions_test;
           start_input_functions_test;
           start_output_functions_test;
         ]

let _ = run_test_tt_main suite
