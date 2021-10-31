open Game
open TurnState

(**Initialize a starting turn state [start_state].*)
let start_state = create_state (create_plist [ "Player 1"; "Player 2" ])

(**[get_first pred] is a helper function.*)
let get_first pred = List.find pred

(**[print_card_list_aux lst] is a helper function.*)
let rec print_card_list_aux (lst : card list) =
  match lst with
  | [] -> ""
  | h :: t -> match_card h ^ "\n" ^ print_card_list_aux t

(**[print_card_list str lst] prints the card list [lst].*)
let print_card_list str lst =
  print_endline (str ^ print_card_list_aux lst)

(**[proceed_to_river state] continues the Poker game with the turn state
   [river] by producing a new state [river_state].*)
let proceed_to_river state =
  print_endline "\n\nHere is the river \n";

  let river_state = draw_turn_or_river state in

  get_community river_state
  |> print_card_list "\n\nHere is the river: \n";

  get_first (fun t -> get_name t = "Player 1") (get_players river_state)
  |> get_hand
  |> print_card_list "\n\nHere is your hand again \n";

  print_endline
    "\n\n\
     Do you want to check, raise, or fold? [c] to check, [r] to raise, \
     [f] to fold. All other inputs will be considered a check. \n";
  print_string "> ";
  match read_line () with
  | "c" -> print_endline "\n You have checked"
  | "r" ->
      print_endline
        "\n You are raising. How much do you want to raise by?"
  | _ -> print_endline "\n You have folded, want to play another hand?"

(**[proceed_to_turn state] continues the Poker game with the turn state
   [state] by producing a new state [pturn_state].*)
let proceed_to_turn state =
  print_endline "\n\nHere is the turn \n";

  let pturn_state = draw_turn_or_river state in

  get_community pturn_state
  |> print_card_list "\n\nHere is the turn: \n";

  get_first (fun t -> get_name t = "Player 1") (get_players pturn_state)
  |> get_hand
  |> print_card_list "\n\nHere is your hand again \n";

  print_endline
    "\n\n\
     Do you want to check, raise, or fold? [c] to check, [r] to raise, \
     [f] to fold. All other inputs will be considered a check. \n";
  print_string "> ";
  match read_line () with
  | "c" ->
      print_endline "\n You have checked";
      proceed_to_river pturn_state
  | "r" ->
      print_endline
        "\n You are raising. How much do you want to raise by?"
  | _ -> print_endline "\n You have folded, want to play another hand?"

(**[proceed_to_flop state] continues the Poker game with the turn state
   [state] by producing a new state [flop_state].*)
let proceed_to_flop state =
  print_endline "\n\nHere is the flop \n";

  let flop_state = draw_flop state in

  flop_state |> get_community
  |> print_card_list "\n\nHere is the flop: \n";

  get_first (fun t -> get_name t = "Player 1") (get_players flop_state)
  |> get_hand
  |> print_card_list "\n\nHere is your hand again \n";

  print_endline
    "\n\n\
     Do you want to check, raise, or fold? [c] to check, [r] to raise, \
     [f] to fold. All other inputs will be considered a check. \n";
  print_string "> ";
  match read_line () with
  | "c" ->
      print_endline "\n You have checked";
      proceed_to_turn flop_state
  | "r" ->
      print_endline
        "\n You are raising. How much do you want to raise by?"
  | _ -> print_endline "\n You have folded, want to play another hand?"

(** [play_game start_state] starts the game with the turn state
    [start_state]. *)
let play_game start_state =
  print_endline "\n\nA heads up game has begun  \n";

  print_endline "\n\nHere is your hand:  \n";

  Random.self_init ();

  let preflop_state = deal_hands (shuffle_state start_state) in

  get_first
    (fun t -> get_name t = "Player 1")
    (get_players preflop_state)
  |> get_hand
  |> print_card_list "\nHere is your hand: \n";

  print_endline
    "\n\n\
     Do you want to check, raise, or fold? [c] to check, [r] to raise, \
     [f] to fold. All other inputs will be considered a check. \n";
  print_string "> ";
  match read_line () with
  | "c" ->
      print_endline "\n You have checked";
      proceed_to_flop preflop_state
  | "r" ->
      print_endline
        "\n You are raising. How much do you want to raise by?"
  | _ -> print_endline "\n You have folded, want to play another hand?"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Poker engine.\n";
  print_endline
    "Do you want to play a hand? [y/_] Anything other than [y] will be \
     interpretted as no.\n";
  print_string "> ";
  match read_line () with
  | "y" -> play_game start_state
  | _ -> ()

(* Execute the game engine. *)
let () = main ()
