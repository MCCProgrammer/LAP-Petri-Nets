open Petrinets
open OUnit2
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

let mk_p1 () = 
  empty 
  |> add_place idle 1 1
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

  |> add_arc (mk_outgoing_arc idle insert_coin 1)
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

(*-------- BUILD ---------- *)

let test_building = "testing building a coffeemachine" >:: (fun _ -> 
  match mk_p1 () with
  | None -> assert_failure "Failed to build the coffeemachine"
  | Some p ->       
    assert_equal (marking_of_place idle p) (Some 1);
    assert_equal (marking_of_place coin_inserted p) (Some 0);
    assert_equal (marking_of_place choice_made p) (Some 0);
    assert_equal (marking_of_place water_heated p) (Some 0);
    assert_equal (marking_of_place grain_ground p) (Some 0);
    assert_equal (marking_of_place coffee_ready p) (Some 0);
    assert_bool "Should be enabled" (is_enabled insert_coin p);
    assert_bool "Should be disabled" (not (is_enabled make_choice p));
    assert_bool "Should be disabled" (not (is_enabled heat_water p));
    assert_bool "Should be disabled" (not (is_enabled grind_coffee p));
    assert_bool "Should be disabled" (not (is_enabled brew_coffee p));
    assert_bool "Should be disabled" (not (is_enabled remove_cup p));
)

(*-------- FIRE ---------- *)

let fire_step1 () = 
  mk_p1 () 
  |>> fire insert_coin 

let fire_step2 () =
  fire_step1 ()
  |>> fire make_choice

let fire_step3 () =
  fire_step2 ()
  |>> fire heat_water

let fire_step4 () =
  fire_step3 ()
  |>> fire grind_coffee

let fire_step5 () =
  fire_step4 ()
  |>> fire brew_coffee

let fire_step6 () =
  fire_step5 ()
  |>> fire remove_cup

let test_firing_step1 = "testing firing step 1" >:: (fun _ -> 
  let p = fire_step1 () in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place idle p) (Some 0);
    assert_equal (marking_of_place coin_inserted p) (Some 1);
    assert_equal (marking_of_place choice_made p) (Some 0);
    assert_equal (marking_of_place water_heated p) (Some 0);
    assert_equal (marking_of_place grain_ground p) (Some 0);
    assert_equal (marking_of_place coffee_ready p) (Some 0);
    assert_bool "Should be disabled" (not (is_enabled insert_coin p));
    assert_bool "Should be enabled" (is_enabled make_choice p);
)
let test_firing_step2 = "testing firing step 2" >:: (fun _ -> 
  let p = fire_step2 () in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place idle p) (Some 0);
    assert_equal (marking_of_place coin_inserted p) (Some 0);
    assert_equal (marking_of_place choice_made p) (Some 2);
    assert_equal (marking_of_place water_heated p) (Some 0);
    assert_equal (marking_of_place grain_ground p) (Some 0);
    assert_equal (marking_of_place coffee_ready p) (Some 0);
    assert_bool "Should be disabled" (not (is_enabled insert_coin p));
    assert_bool "Should be disabled" (not (is_enabled make_choice p));
    assert_bool "Should be enabled" (is_enabled heat_water p);
    assert_bool "Should be enabled" (is_enabled grind_coffee p);
)
let test_firing_step3 = "testing firing step 3" >:: (fun _ -> 
  let p = fire_step3 () in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place idle p) (Some 0);
    assert_equal (marking_of_place coin_inserted p) (Some 0);
    assert_equal (marking_of_place choice_made p) (Some 1);
    assert_equal (marking_of_place water_heated p) (Some 1);
    assert_equal (marking_of_place grain_ground p) (Some 0);
    assert_equal (marking_of_place coffee_ready p) (Some 0);
    assert_bool "Should be disabled" (not (is_enabled insert_coin p));
    assert_bool "Should be disabled" (not (is_enabled make_choice p));
    assert_bool "Should be disabled" (not (is_enabled heat_water p));
    assert_bool "Should be enabled" (is_enabled grind_coffee p);
)
let test_firing_step4 = "testing firing step 4" >:: (fun _ -> 
  let p = fire_step4 () in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place idle p) (Some 0);
    assert_equal (marking_of_place coin_inserted p) (Some 0);
    assert_equal (marking_of_place choice_made p) (Some 0);
    assert_equal (marking_of_place water_heated p) (Some 1);
    assert_equal (marking_of_place grain_ground p) (Some 1);
    assert_equal (marking_of_place coffee_ready p) (Some 0);
    assert_bool "Should be disabled" (not (is_enabled insert_coin p));
    assert_bool "Should be disabled" (not (is_enabled make_choice p));
    assert_bool "Should be disabled" (not (is_enabled heat_water p));
    assert_bool "Should be disabled" (not (is_enabled grind_coffee p));
    assert_bool "Should be enabled" (is_enabled brew_coffee p);
)
let test_firing_step5 = "testing firing step 5" >:: (fun _ -> 
  let p = fire_step5 () in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place idle p) (Some 0);
    assert_equal (marking_of_place coin_inserted p) (Some 0);
    assert_equal (marking_of_place choice_made p) (Some 0);
    assert_equal (marking_of_place water_heated p) (Some 0);
    assert_equal (marking_of_place grain_ground p) (Some 0);
    assert_equal (marking_of_place coffee_ready p) (Some 1);
    assert_bool "Should be disabled" (not (is_enabled insert_coin p));
    assert_bool "Should be disabled" (not (is_enabled make_choice p));
    assert_bool "Should be disabled" (not (is_enabled heat_water p));
    assert_bool "Should be disabled" (not (is_enabled grind_coffee p));
    assert_bool "Should be disabled" (not (is_enabled brew_coffee p));
    assert_bool "Should be enabled" (is_enabled remove_cup p);
)
let test_firing_step6 = "testing firing step 6" >:: (fun _ -> 
  let p = fire_step6 () in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place idle p) (Some 1);
    assert_equal (marking_of_place coin_inserted p) (Some 0);
    assert_equal (marking_of_place choice_made p) (Some 0);
    assert_equal (marking_of_place water_heated p) (Some 0);
    assert_equal (marking_of_place grain_ground p) (Some 0);
    assert_equal (marking_of_place coffee_ready p) (Some 0);
    assert_bool "Should be enabled" (is_enabled insert_coin p);
    assert_bool "Should be disabled" (not (is_enabled make_choice p));
    assert_bool "Should be disabled" (not (is_enabled heat_water p));
    assert_bool "Should be disabled" (not (is_enabled grind_coffee p));
    assert_bool "Should be disabled" (not (is_enabled brew_coffee p));
    assert_bool "Should be disabled" (not (is_enabled remove_cup p));
)

let tests_coffeemachine = [
  test_building
  ; test_firing_step1
  ; test_firing_step2
  ; test_firing_step3
  ; test_firing_step4
  ; test_firing_step5
  ; test_firing_step6
];;

let test_suite_coffeemachine = "test suite for petrinets" >::: tests_coffeemachine
