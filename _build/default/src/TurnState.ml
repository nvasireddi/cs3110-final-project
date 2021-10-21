type card = {
  value : int;
  suit : string;
}
(**The type [card] is a record representing a card in a standard deck,
   with fields [value] of type int and [suit] of type string*)

type player = {
  name : string;
  hand : card list;
}
(**The type [player] is a record representing a player in a poker game,
   with fields [name] of type string and [hand] of type card list*)

(**Initialize a standard deck of 52 cards, with a value of [1]
   representing Ace, and [11],[12],[13] representing Jack, Queen, King,
   respectively.*)
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
(**The type t represents the turn state of the round of poker. It
   contains [players] representing a list of players, [community_cards]
   representing a list of cards on the table but not in player hands,
   and [current_deck] representing the deck used in the game.*)

(**[draw_to_community_card turn_state n] is a helper function that
   removes [n] cards from the current_deck of [turn_state], and moves
   those cards into the community_cards of [turn_state]*)
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

(**[draw_flop turn_state] uses [draw_to_community_card] to create a
   poker flop, returning an updated turn state with the flop.*)
let draw_flop (turn_state : t) = draw_to_community_card turn_state 3

(**[draw_turn_or_river turn_state] uses [draw_to_community_card] to
   create a poker turn or river, returning an updated turn state with
   the turn or river.*)
let draw_turn_or_river (turn_state : t) =
  draw_to_community_card turn_state 1

(**[shuffle lst] is a helper function that shuffles a list [lst]*)
let rec shuffle = function
  | [] -> []
  | [ x ] -> [ x ]
  | list ->
      let before, after =
        List.partition (fun element -> Random.bool ()) list
      in
      List.rev_append (shuffle before) (shuffle after)

(**[shuffle_state turn_state] uses [shuffle] to shuffle the deck of
   [turn_state], returning the updated turn state.*)
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

(**[deal_2 deck] returns the top 2 cards of the [deck] as a list.*)
let deal_2 (deck : card list) =
  match deck with
  | h1 :: h2 :: _ -> [ h1; h2 ]
  | _ -> []

(**[deal_hand_to_player p deck] updates the player [p]'s hand with the
   cards dealt from [deal_2 deck].*)
let deal_hand_to_player (p : player) (deck : card list) =
  { name = p.name; hand = deal_2 deck }

(**[remove_2 deck] returns the [deck] with the top 2 cards removed, as a
   list.*)
let remove_2 (deck : card list) =
  match deck with
  | _ :: _ :: t -> t
  | _ -> []

(**[remove_2n deck n] removes 2*n cards from [deck], returning the
   remaining deck.*)
let rec remove_2n (deck : card list) (n : int) =
  if n > 0 then remove_2n (remove_2 deck) (n - 1) else deck

(**deal_hands_aux plist deck] deals out hands to the players in [plist].*)
let rec deal_hands_aux (plist : player list) (deck : card list) =
  match plist with
  | [ h ] -> [ deal_hand_to_player h deck ]
  | h :: t ->
      deal_hand_to_player h deck :: deal_hands_aux t (remove_2 deck)
  | _ -> []

(**[deal_hands turn_state] returns an updated turn state after dealing
   out hands to all of the players in [turn_state], and ensuring the
   cards dealt out have been removed from the deck of [turn_state].*)
let deal_hands turn_state =
  {
    players = deal_hands_aux turn_state.players turn_state.current_deck;
    community_cards = turn_state.community_cards;
    current_deck =
      remove_2n turn_state.current_deck (List.length turn_state.players);
  }
