type card = {
  value : int;
  suit : string;
}

type player = {
  name : string;
  hand : card list;
}

let (deck : card list) =
  [
    { value = 1; suit = "Spades" };
    { value = 2; suit = "Spades" };
    { value = 3; suit = "Spades" };
    { value = 4; suit = "Spades" };
    { value = 5; suit = "Spades" };
    { value = 6; suit = "Spades" };
    { value = 7; suit = "Spades" };
    { value = 8; suit = "Spades" };
    { value = 9; suit = "Spades" };
    { value = 10; suit = "Spades" };
    { value = 11; suit = "Spades" };
    { value = 12; suit = "Spades" };
    { value = 13; suit = "Spades" };
    { value = 1; suit = "Hearts" };
    { value = 2; suit = "Hearts" };
    { value = 3; suit = "Hearts" };
    { value = 4; suit = "Hearts" };
    { value = 5; suit = "Hearts" };
    { value = 6; suit = "Hearts" };
    { value = 7; suit = "Hearts" };
    { value = 8; suit = "Hearts" };
    { value = 9; suit = "Hearts" };
    { value = 10; suit = "Hearts" };
    { value = 11; suit = "Hearts" };
    { value = 12; suit = "Hearts" };
    { value = 13; suit = "Hearts" };
    { value = 1; suit = "Clubs" };
    { value = 2; suit = "Clubs" };
    { value = 3; suit = "Clubs" };
    { value = 4; suit = "Clubs" };
    { value = 5; suit = "Clubs" };
    { value = 6; suit = "Clubs" };
    { value = 7; suit = "Clubs" };
    { value = 8; suit = "Clubs" };
    { value = 9; suit = "Clubs" };
    { value = 10; suit = "Clubs" };
    { value = 11; suit = "Clubs" };
    { value = 12; suit = "Clubs" };
    { value = 13; suit = "Clubs" };
    { value = 1; suit = "Diamonds" };
    { value = 2; suit = "Diamonds" };
    { value = 3; suit = "Diamonds" };
    { value = 4; suit = "Diamonds" };
    { value = 5; suit = "Diamonds" };
    { value = 6; suit = "Diamonds" };
    { value = 7; suit = "Diamonds" };
    { value = 8; suit = "Diamonds" };
    { value = 9; suit = "Diamonds" };
    { value = 10; suit = "Diamonds" };
    { value = 11; suit = "Diamonds" };
    { value = 12; suit = "Diamonds" };
    { value = 13; suit = "Diamonds" };
  ]

type t = {
  players : player list;
  community_cards : card list;
  current_deck : card list;
}

let rec draw_to_community_card (turn_state : t) (n : int) : t =
  let new_state =
    {
      players = turn_state.players;
      community_cards =
        List.hd turn_state.current_deck :: turn_state.community_cards;
      current_deck = List.tl turn_state.current_deck;
    }
  in
  if n = 1 then new_state else draw_to_community_card new_state (n - 1)

let draw_flop (turn_state : t) = draw_to_community_card turn_state 3

let draw_turn_or_river (turn_state : t) =
  draw_to_community_card turn_state 1

(* check algo for copying *)
let rec shuffle = function
  | [] -> []
  | [ x ] -> [ x ]
  | list ->
      let before, after =
        List.partition (fun element -> Random.bool ()) list
      in
      List.rev_append (shuffle before) (shuffle after)

let rec shuffle_state turn_state =
  match turn_state.current_deck with
  | [] -> turn_state
  | [ x ] -> turn_state
  | list ->
      {
        players = turn_state.players;
        community_cards = turn_state.community_cards;
        current_deck = shuffle turn_state.current_deck;
      }

let deal_2 (deck : card list) =
  match deck with
  | h1 :: h2 :: _ -> [ h1; h2 ]
  | _ -> []

let deal_hand_to_player (p : player) (deck : card list) =
  { name = p.name; hand = deal_2 deck }

let remove_2 (deck : card list) =
  match deck with
  | _ :: _ :: t -> t
  | _ -> []

let rec remove_2n (deck : card list) (n : int) =
  if n > 0 then remove_2n (remove_2 deck) (n - 1) else deck

let rec deal_hands_aux (plist : player list) (deck : card list) =
  match plist with
  | [ h ] -> [ deal_hand_to_player h deck ]
  | h :: t ->
      deal_hand_to_player h deck :: deal_hands_aux t (remove_2 deck)
  | _ -> []

let deal_hands turn_state =
  {
    players = deal_hands_aux turn_state.players turn_state.current_deck;
    community_cards = turn_state.community_cards;
    current_deck =
      remove_2n turn_state.current_deck (List.length turn_state.players);
  }
