open OUnit2
open Petrinets
open Utils

(*------------ TESTS --------------*)


(*------------ BUILD --------------*)
let a = mk_place "A" 
let b = mk_place "B" 
let t = mk_transition "T"
let mk_p0 () = empty |> add_place a 1 1
                      |> add_place b 1 0
                      |> add_transition t
                      |> add_arc (mk_outgoing_arc a t 1)
                      |>> add_arc (mk_incoming_arc t b 1)

let test_building = "testing building a petri net" >:: (fun _ -> 
  match mk_p0 () with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place a p) (Some 1);
    assert_equal (marking_of_place b p) (Some 0);
    assert_bool "Should be enabled" (is_enabled t p);
)

(*------------ FIRE --------------*)


let test_firing = "testing firing a transition" >:: (fun _ -> 
  let p = mk_p0() |>> fire t 
  in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place a p) (Some 0);
    assert_equal (marking_of_place b p) (Some 1);
    assert_bool "Should be disabled" (not (is_enabled t p))
)

(*----------TRANSITIONS: N TOKENS-------------*)
let mk_p2() = empty |> add_place a 2 2
                    |> add_place b 2 0
                    |> add_transition t
                    |> add_arc (mk_outgoing_arc a t 2)
                    |>> add_arc (mk_incoming_arc t b 2)

let test_building2 = "testing building a petri net with 2 tokens in places and transitions" >:: (fun _ -> 
  match mk_p2 () with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place a p) (Some 2);
    assert_equal (marking_of_place b p) (Some 0);
    assert_bool "Should be enabled" (is_enabled t p);
)

(*------------ FIRE WITH N TOKENS--------------*)


let test_firing2 = "testing firing a transition with 2-2 tokens" >:: (fun _ -> 
  let p = mk_p2() |>> fire t 
  in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place a p) (Some 0);
    assert_equal (marking_of_place b p) (Some 2);
    assert_bool "Should be disabled" (not (is_enabled t p))
)
 
(*----------TRANSITIONS: N TOKENS-------------*)

let mk_p2_1() = empty |> add_place a 1 1
                      |> add_place b 2 0
                      |> add_transition t
                      |> add_arc (mk_outgoing_arc a t 1)
                      |>> add_arc (mk_incoming_arc t b 2)

let test_firing2_1 = "testing firing a transition with 1-2 tokens" >:: (fun _ -> 
  let p = mk_p2_1() |>> fire t 
  in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place a p) (Some 0);
    assert_equal (marking_of_place b p) (Some 2);
    assert_bool "Should be disabled" (not (is_enabled t p))
)

(*----------TRANSITIONS: N TOKENS-------------*)

let mk_p2_2() = empty |> add_place a 2 2
                      |> add_place b 1 0
                      |> add_transition t
                      |> add_arc (mk_outgoing_arc a t 2)
                      |>> add_arc (mk_incoming_arc t b 1)

let test_firing2_2 = "testing firing a transition with 1-2 tokens" >:: (fun _ -> 
  let p = mk_p2_2() |>> fire t 
  in
  match p with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place a p) (Some 0);
    assert_equal (marking_of_place b p) (Some 1);
    assert_bool "Should be disabled" (not (is_enabled t p))
)

(*----------TRANSITIONS: CAPACITY-------------*)
let mk_p3() = empty |> add_place a 1 1
                    |> add_place b 1 0
                    |> add_transition t
                    |> add_arc (mk_outgoing_arc a t 1)
                    |>> add_arc (mk_incoming_arc t b 2)

let test_building3 = "testing building a petri net with 2 tokens in transitions and not enough capacity" >:: (fun _ -> 
  match mk_p3 () with
  | None -> assert_failure "Failed to build the petri net"
  | Some p ->       
    assert_equal (marking_of_place a p) (Some 1);
    assert_equal (marking_of_place b p) (Some 0);
    assert_bool "Should not be enabled" (not (is_enabled t p));
)
let test_firing3 = "testing firing a transition without " >:: (fun _ -> 
  let p = mk_p3() |>> fire t 
  in
  assert_equal p None;
)

let tests = [
    test_building
  ; test_firing
  ; test_building2
  ; test_firing2
  ; test_firing2_1
  ; test_firing2_2
  ; test_building3
  ; test_firing3
  (* ; test_firingall
  ; is_stuck_test *)
];;

let test_suite_buildfire = "test suite for petrinets" >::: tests;;
