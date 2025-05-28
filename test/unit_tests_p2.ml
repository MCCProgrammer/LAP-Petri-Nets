open OUnit2
open Utils
open Petrinets

let canary_test = "testing tests" >:: (fun _ -> assert_equal 1 1)

let test_empty = "testing empty net" >:: (fun _ ->
  let p = empty in
  assert_equal (places p) [];
  assert_equal (transitions p) [];
  assert_equal (arcs p) [];
)

(**********************************************************************)
(*                        TESTING PLACES                              *)
(**********************************************************************)

let test_add_place1 = "testing add place - one place" >:: (fun _ ->
  let p = mk_place "A" in
  let n = add_place p 1 0 empty in 
  let l = places n in 
  assert_bool "places should have length 1" 
    (List.length l = 1); 
  assert_bool "place p should be in the net's place list" 
    (List.exists (fun x -> (compare_places x p) = 0) l);
)

let test_add_place2 = "testing add place - two places" >:: (fun _ ->
  let p1 = mk_place "A" in
  let p2 = mk_place "B" in 
  let n = empty |> add_place p1 1 0 
                |> add_place p2 1 0 in  
  let l = places n in 
  assert_bool "places should have length 2" 
    (List.length l = 2); 
  assert_bool "place p1 should be in the net's place list" 
    (List.exists (fun x -> (compare_places x p1) = 0) l);
  assert_bool "place p2 should be in the net's place list" 
    (List.exists (fun x -> (compare_places x p2) = 0) l);
)

let test_add_place3 = "testing add place - two equal places" >:: (fun _ ->
  let p = mk_place "A" in
  let n = empty |> add_place p 1 0 |> add_place p 1 0 in
  let l = places n in 
  assert_bool "places should have length 1" 
    (List.length l = 1); 
  assert_bool "place p1 should be in the net's place list" 
    (List.exists (fun x -> (compare_places x p) = 0) l);
)

let test_add_place4 = "testing add place - two equal places; diff capacity" >:: (fun _ ->
  let p = mk_place "A" in
  let n = empty |> add_place p 1 0 |> add_place p 2 0 in
  let l = places n in 
  assert_bool "places should have length 1" 
    (List.length l = 1);
  assert_bool "place p1 should be in the net's place list" 
    (List.exists (fun x -> (compare_places x p) = 0) l);
)

(**********************************************************************)
(*                      TESTING TRANSITIONS                           *)
(**********************************************************************)

let test_add_transition1 = "testing add transition" >:: (fun _ ->
  let t = mk_transition "A" in
  let n = empty |> add_transition t in
  let l = transitions n in 
  assert_bool "transitions should have length 1" 
    (List.length l = 1);
  assert_bool "transition t should be in the net's transition list" 
    (List.exists (fun x -> (compare_transitions x t) = 0) l);
)

let test_add_transition2 = "testing add transition - two diff transitions" >:: (fun _ ->
  let t1 = mk_transition "A" in
  let t2 = mk_transition "B" in 
  let n = empty |> add_transition t1  
                |> add_transition t2 in  
  let l = transitions n in 
  assert_bool "transitions should have length 2" 
    (List.length l = 2); 
  assert_bool "transition t1 should be in the net's transition list" 
    (List.exists (fun x -> (compare_transitions x t1) = 0) l);
  assert_bool "transition t2 should be in the net's transition list" 
    (List.exists (fun x -> (compare_transitions x t2) = 0) l);
)

let test_add_transition3 = "testing add transition - two equal transitions" >:: (fun _ ->
  let t1 = mk_transition "A" in 
  let n = empty |> add_transition t1  
                |> add_transition t1 in  
  let l = transitions n in 
  assert_bool "transitions should have length 1" 
    (List.length l = 1); 
  assert_bool "transition t1 should be in the net's transition list" 
    (List.exists (fun x -> (compare_transitions x t1) = 0) l);
)

(**********************************************************************)
(*                          TESTING ARCS                              *)
(**********************************************************************)

let test_add_out_arc1 = "testing add outgoing arc" >:: (fun _ ->
  let p = mk_place "A" in
  let t = mk_transition "T" in 
  let a = mk_outgoing_arc p t 1 in
  let _ = empty |> add_place p 1 0 |> add_transition t 
                |> add_arc a 
                |> function
                  | None -> assert_failure "Failed to add arc" 
                  | Some net -> 
                    let l = arcs net in 
                      assert_bool "arcs should have length 1" 
                        (List.length l = 1);
                      assert_bool "arc 'a' should be in the net's arcs list" 
                        (List.exists (fun x -> (compare_arcs x a) = 0) l); 
  in ()
) 

let test_add_out_arc2 = "testing add outgoing arc - place not in net" >:: (fun _ ->
  let p3 = mk_place "C" in
  let t = mk_transition "T" in 
  let a = mk_outgoing_arc p3 t 1 in
  let _ = empty |> add_transition t 
                |> add_arc a 
                |> function
                  | None -> ()
                  | Some _ -> assert_failure "Should not have succeeded." 
  in ()
) 

let test_add_out_arc3 = "testing add outgoing arc - transition not in net" >:: (fun _ ->
  let p1 = mk_place "A" in
  let t1 = mk_transition "T1" in 
  let a = mk_outgoing_arc p1 t1 1 in
  let _ = empty |> add_place p1 1 0
                |> add_arc a 
                |> function
                  | None -> ()
                  | Some _ -> assert_failure "Should not have succeeded." 
  in ()
) 

let test_add_in_arc1 = "testing add incoming arc" >:: (fun _ ->
  let p = mk_place "A" in
  let t = mk_transition "T" in 
  let a = mk_incoming_arc t p 1 in
  let _ = empty |> add_place p 1 0 |> add_transition t 
                |> add_arc a 
                |> function
                  | None -> assert_failure "Failed to add arc" 
                  | Some net -> 
                    let l = arcs net in 
                      assert_bool "arcs should have length 1" 
                        (List.length l = 1);
                      assert_bool "arc 'a' should be in the net's arcs list" 
                        (List.exists (fun x -> (compare_arcs x a) = 0) l); 
  in ()
) 

let test_add_in_arc2 = "testing add incoming arc - place not in net" >:: (fun _ ->
  let p = mk_place "A" in
  let t = mk_transition "T" in
  let a = mk_incoming_arc t p 1 in
  let _ = empty |> add_transition t 
                |> add_arc a
                |> function
                  | None -> ()
                  | Some _ -> assert_failure "Should not have succeeded." 
  in ()
) 

let test_add_in_arc3 = "testing add outgoing arc - transition not in net" >:: (fun _ ->
  let p = mk_place "A" in
  let t = mk_transition "T1" in 
  let a = mk_incoming_arc t p 1 in
  let _ = empty |> add_place p 1 0
                |> add_arc a 
                |> function
                  | None -> ()
                  | Some _ -> assert_failure "Should not have succeeded." 
  in ()
) 

(**********************************************************************)
(*                  TESTING ENABLED TRANSITIONS                       *)
(**********************************************************************)

let test_is_enabled1 = "testing is enabled - outgoing arc, but source place has 0 tokens" >:: (fun _ ->
  let p1 = mk_place "A" in
  let p2 = mk_place "B" in
  let t = mk_transition "T" in
  let o = mk_outgoing_arc p1 t 1 in 
  let i = mk_incoming_arc t p2 1 in
  let n = empty |> add_place p1 1 0 
                |> add_place p2 1 1 
                |> add_transition t 
                |> add_arc o 
                |>> add_arc i
  in 
    match n with
    | None -> assert_failure "Failed to build the petri net"
    | Some n -> 
      let updated_t = List.find (fun e -> compare_transitions e t = 0) (transitions n) in 
      assert_bool "place should be in the net" (List.length (places n) = 2);
      assert_bool "transition should be in the net" (List.length (transitions n) = 1);
      assert_bool "arc should be in the net" (List.length (arcs n) = 2);
      assert_bool "Should be disabled" (not (is_enabled updated_t n));
)

let test_is_enabled2 = "testing is enabled - incoming arc (destination no capacity left)" >:: (fun _ ->
  let p1 = mk_place "A" in
  let p2 = mk_place "B" in
  let t = mk_transition "T" in
  let o = mk_outgoing_arc p1 t 1 in 
  let i = mk_incoming_arc t p2 1 in
  let n = empty |> add_place p1 1 1 
                |> add_place p2 1 1 
                |> add_transition t 
                |> add_arc o 
                |>> add_arc i
  in 
    match n with
    | None -> assert_failure "Failed to build the petri net"
    | Some n -> 
      let updated_t = List.find (fun e -> compare_transitions e t = 0) (transitions n) in 
      assert_bool "place should be in the net" (List.length (places n) = 2);
      assert_bool "transition should be in the net" (List.length (transitions n) = 1);
      assert_bool "arc should be in the net" (List.length (arcs n) = 2);
      assert_bool "Should be disabled" (not (is_enabled updated_t n));
)

let test_is_enabled3 = "testing is enabled - incoming arc (destination with available capacity)" >:: (fun _ ->
  let p1 = mk_place "A" in
  let p2 = mk_place "B" in
  let t = mk_transition "T" in
  let o = mk_outgoing_arc p1 t 1 in 
  let i = mk_incoming_arc t p2 1 in
  let n = empty |> add_place p1 1 1 
                |> add_place p2 2 1 
                |> add_transition t 
                |> add_arc o 
                |>> add_arc i
  in 
    match n with
    | None -> assert_failure "Failed to build the petri net"
    | Some n -> 
      let updated_t = List.find (fun e -> compare_transitions e t = 0) (transitions n) in 
      assert_bool "place should be in the net" (List.length (places n) = 2);
      assert_bool "transition should be in the net" (List.length (transitions n) = 1);
      assert_bool "arc should be in the net" (List.length (arcs n) = 2);
      assert_bool "Should be enabled" (is_enabled updated_t n);
)

let test_is_enabled4 = "testing is enabled - outgoing arc (not enough capacity)" >:: (fun _ ->
  let p1 = mk_place "A" in
  let p2 = mk_place "B" in
  let t = mk_transition "T" in
  let o = mk_outgoing_arc p1 t 2 in (* requires 2 tokens *)
  let i = mk_incoming_arc t p2 1 in
  let n = empty |> add_place p1 2 1 (* only 1 token *)
                |> add_place p2 2 1 
                |> add_transition t 
                |> add_arc o 
                |>> add_arc i
  in 
    match n with
    | None -> assert_failure "Failed to build the petri net"
    | Some n -> 
      let updated_t = List.find (fun e -> compare_transitions e t = 0) (transitions n) in 
      assert_bool "place should be in the net" (List.length (places n) = 2);
      assert_bool "transition should be in the net" (List.length (transitions n) = 1);
      assert_bool "arc should be in the net" (List.length (arcs n) = 2);
      assert_bool "Should be disabled" (not (is_enabled updated_t n));
)

let test_is_enabled5 = "testing is enabled - incoming arc (not enough capacity)" >:: (fun _ ->
  let p1 = mk_place "A" in
  let p2 = mk_place "B" in
  let t = mk_transition "T" in
  let o = mk_outgoing_arc p1 t 1 in 
  let i = mk_incoming_arc t p2 2 in
  let n = empty |> add_place p1 1 1 
                |> add_place p2 2 1 
                |> add_transition t 
                |> add_arc o 
                |>> add_arc i
  in 
    match n with
    | None -> assert_failure "Failed to build the petri net"
    | Some n -> 
      let updated_t = List.find (fun e -> compare_transitions e t = 0) (transitions n) in 
      assert_bool "place should be in the net" (List.length (places n) = 2);
      assert_bool "transition should be in the net" (List.length (transitions n) = 1);
      assert_bool "arc should be in the net" (List.length (arcs n) = 2);
      assert_bool "Should be disabled" (not (is_enabled updated_t n));
)

let test_is_enabled6 = "testing is enabled - incoming arc does not exist" >:: (fun _ ->
  let p = mk_place "A" in
  let t = mk_transition "T" in
  let o = mk_outgoing_arc p t 1 in 
  let n = empty |> add_place p 1 1 
                |> add_transition t 
                |> add_arc o 
  in 
    match n with
    | None -> assert_failure "Failed to build the petri net"
    | Some n -> 
      let updated_t = List.find (fun e -> compare_transitions e t = 0) (transitions n) in 
      assert_bool "place should be in the net" (List.length (places n) = 1);
      assert_bool "transition should be in the net" (List.length (transitions n) = 1);
      assert_bool "arc should be in the net" (List.length (arcs n) = 1);
      assert_bool "Should be disabled" (not (is_enabled updated_t n));
)

let test_is_enabled7 = "testing is enabled - outgoing arc does not exist" >:: (fun _ ->
  let p = mk_place "A" in
  let t = mk_transition "T" in
  let o = mk_incoming_arc t p 1 in 
  let n = empty |> add_place p 1 1 
                |> add_transition t 
                |> add_arc o 
  in 
    match n with
    | None -> assert_failure "Failed to build the petri net"
    | Some n -> 
      let updated_t = List.find (fun e -> compare_transitions e t = 0) (transitions n) in 
      assert_bool "place should be in the net" (List.length (places n) = 1);
      assert_bool "transition should be in the net" (List.length (transitions n) = 1);
      assert_bool "arc should be in the net" (List.length (arcs n) = 1);
      assert_bool "Should be disabled" (not (is_enabled updated_t n));
)

let test_marking_of_place1 = "testing marking of place" >:: (fun _ -> 
  let p = mk_place "A" in
  let n = empty |> add_place p 1 0 in 
    assert_bool "place should be in the net" 
      (List.length (places n) = 1);
    let updated_p = List.find (fun place -> compare_places place p = 0) (places n) in
    let _ = 
      match marking_of_place updated_p n with 
      | None -> assert_failure "Failed to find the place"
      | Some m -> assert_bool "place should have marking of 1" (m = 0); 
    in
    ()
)

let test_marking_of_place2 = "testing marking of place" >:: (fun _ -> 
  let p = mk_place "A" in
  let n = empty |> add_place p 1 1 in 
    assert_bool "place should be in the net" 
      (List.length (places n) = 1);
    let updated_p = List.find (fun place -> compare_places place p = 0) (places n) in
    let _ = 
      match marking_of_place updated_p n with 
      | None -> assert_failure "Failed to find the place"
      | Some m -> assert_bool "place should have marking of 1" (m = 1); 
    in
    ()
)

let tests = [
  canary_test
  ; test_empty
  ; test_add_place1
  ; test_add_place2
  ; test_add_place3
  ; test_add_place4
  ; test_add_transition1
  ; test_add_transition2
  ; test_add_transition3
  ; test_add_out_arc1
  ; test_add_out_arc2
  ; test_add_out_arc3
  ; test_add_in_arc1
  ; test_add_in_arc2
  ; test_add_in_arc3
  ; test_is_enabled1
  ; test_is_enabled2
  ; test_is_enabled3
  ; test_is_enabled4
  ; test_is_enabled5
  ; test_is_enabled6
  ; test_is_enabled7
  ; test_marking_of_place1
  ; test_marking_of_place2 
]

let test_suite_unit_tests_p2 = "Unit tests for P2" >::: tests;;