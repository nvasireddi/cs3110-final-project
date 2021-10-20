open Game
(** [play_game deck] starts the game with the deck deck. *)

open TurnState

let start_state =
  {
    players =
      [
        { name = "Player 1"; hand = [] };
        { name = "Player 2"; hand = [] };
      ];
    community_cards = [];
    current_deck = TurnState.deck;
  }

let get_first pred = List.find pred

let match_card card =
  match card with
  | { value = 1; suit = s } -> "Ace of " ^ s
  | { value = 2; suit = s } -> "2 of " ^ s
  | { value = 3; suit = s } -> "3 of " ^ s
  | { value = 4; suit = s } -> "4 of " ^ s
  | { value = 5; suit = s } -> "5 of " ^ s
  | { value = 6; suit = s } -> "6 of " ^ s
  | { value = 7; suit = s } -> "7 of " ^ s
  | { value = 8; suit = s } -> "8 of " ^ s
  | { value = 9; suit = s } -> "9 of " ^ s
  | { value = 10; suit = s } -> "10 of " ^ s
  | { value = 11; suit = s } -> "Jack of " ^ s
  | { value = 12; suit = s } -> "Queen of " ^ s
  | { value = 13; suit = s } -> "King of " ^ s
  | _ -> "Should not see this"

let rec print_card_list_aux (lst : card list) =
  match lst with
  | [] -> ""
  | h :: t -> match_card h ^ "\n" ^ print_card_list_aux t

let print_card_list str lst =
  print_endline (str ^ print_card_list_aux lst)

let proceed_to_river state =
  print_endline "\n\nHere is the river \n";

  let river_state = draw_turn_or_river state in

  river_state.community_cards
  |> print_card_list "\n\nHere is the river: \n";

  (get_first (fun t -> t.name = "Player 1") river_state.players).hand
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

let proceed_to_turn state =
  print_endline "\n\nHere is the turn \n";

  let pturn_state = draw_turn_or_river state in

  pturn_state.community_cards
  |> print_card_list "\n\nHere is the turn: \n";

  (get_first (fun t -> t.name = "Player 1") pturn_state.players).hand
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

let proceed_to_flop state =
  print_endline "\n\nHere is the flop \n";

  let flop_state = draw_flop state in

  flop_state.community_cards
  |> print_card_list "\n\nHere is the flop: \n";

  (get_first (fun t -> t.name = "Player 1") flop_state.players).hand
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

let play_game start_state =
  print_endline "\n\nA heads up game has begun  \n";

  print_endline "\n\nHere is your hand:  \n";

  let preflop_state = deal_hands (shuffle_state start_state) in

  (get_first (fun t -> t.name = "Player 1") preflop_state.players).hand
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
