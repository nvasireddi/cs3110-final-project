type t
(**The abstract type of values representing a turn state*)

type card

type player

val create_state : player list -> t
(**[create_state plist clist cdeck] returns a turn state of type [t]
   with fields [plist], [clist], and [cdeck]*)

val create_plist : string list -> player list
(**[create_plist slst] returns a player list, where each player has a
   name in [slst] and an empty hand*)

val deck : card list
(**Initialize a standard deck of 52 cards, with a value of [1]
   representing Ace, and [11],[12],[13] representing Jack, Queen, King,
   respectively.*)

val draw_flop : t -> t
(**[draw_flop turn_state] uses [draw_to_community_card] to create a
   poker flop, returning an updated turn state with the flop.*)

val draw_turn_or_river : t -> t
(**[draw_turn_or_river turn_state] uses [draw_to_community_card] to
   create a poker turn or river, returning an updated turn state with
   the turn or river.*)

val shuffle_state : t -> t
(**[shuffle_state turn_state] uses [shuffle] to shuffle the deck of
   [turn_state], returning the updated turn state.*)

val deal_hands : t -> t
(**[deal_hands turn_state] returns an updated turn state after dealing
   out hands to all of the players in [turn_state], and ensuring the
   cards dealt out have been removed from the deck of [turn_state].*)

val match_card : card -> string
(**[match_card card] returns the string representation of the card
   [card]*)

val get_community : t -> card list
(**[get_community turn_state] returns the community card list of
   [turn_state]*)

val get_name : player -> string
(**[get_name player] returns the name of [player] as a string*)

val get_players : t -> player list
(**[get_players turn_state] returns the player list of [turn_state]*)

val get_hand : player -> card list
(**[get_hand player] returns the hand of [player] as a card list*)