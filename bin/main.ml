open Petrinets
open Utils

let idle = mk_place "Idle"
let coin_inserted = mk_place "Coin Inserted"
let choice_made = mk_place "Choice Made"
let water_heated = mk_place "Water Heated"
let grain_ground = mk_place "Grain Ground"
let coffee_ready = mk_place "Coffee Ready"

let insert_coin = mk_transition "insert coin"
let make_choice = mk_transition "make choice"
let heat_water = mk_transition "heat water"
let grind_coffee = mk_transition "grind coffee"
let brew_coffee = mk_transition "brew coffee"
let remove_cup = mk_transition "remove cup"

let p = empty |> add_place idle 1 1
              |> add_place coin_inserted 1 0
              |> add_place choice_made 2 0
              |> add_place water_heated 1 0
              |> add_place grain_ground 1 0 
              |> add_place coffee_ready 1 0

              |> add_transition insert_coin
              |> add_transition make_choice
              |> add_transition heat_water
              |> add_transition grind_coffee
              |> add_transition brew_coffee
              |> add_transition remove_cup              

let p = p |> add_arc (mk_outgoing_arc idle insert_coin 1)
          |>> add_arc (mk_incoming_arc insert_coin coin_inserted 1)
          |>> add_arc (mk_outgoing_arc coin_inserted make_choice 1)
          |>> add_arc (mk_incoming_arc make_choice choice_made 2)
          |>> add_arc (mk_outgoing_arc choice_made heat_water 1)
          |>> add_arc (mk_incoming_arc heat_water water_heated 1)
          |>> add_arc (mk_outgoing_arc choice_made grind_coffee 1)
          |>> add_arc (mk_incoming_arc grind_coffee grain_ground 1)
          |>> add_arc (mk_outgoing_arc water_heated brew_coffee 1)
          |>> add_arc (mk_outgoing_arc grain_ground brew_coffee 1)
          |>> add_arc (mk_incoming_arc brew_coffee coffee_ready 1)
          |>> add_arc (mk_outgoing_arc coffee_ready remove_cup 1)
          |>> add_arc (mk_incoming_arc remove_cup idle 1)  

let p = p |>> fire insert_coin 
          |>> fire make_choice
          |>> fire heat_water
          |>> fire grind_coffee
          |>> fire brew_coffee
          |>> fire remove_cup
          |>> fire insert_coin
          |>> fire make_choice
          |>> fire grind_coffee
          |>> fire heat_water
          |>> fire brew_coffee
          |>> fire remove_cup 

let _ = 
  match p with 
    None -> print_endline "" 
  | Some p -> if is_enabled insert_coin p then 
      print_endline "Ready to insert coin"
    else 
      print_endline "Oops, not ready to insert coin"
