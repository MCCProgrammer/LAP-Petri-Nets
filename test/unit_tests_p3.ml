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
(*                          TESTING FIRE                              *)
(**********************************************************************)

let test_fire1 = "fire single token transfer" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 1 1
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 1)
              |>> add_arc (mk_incoming_arc t p_out 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->

    assert_equal (Some 1) (marking_of_place p_in net);
    assert_equal (Some 0) (marking_of_place p_out net);

    assert_bool "Transition should be enabled" 
      (is_enabled t net);

    begin match fire t net with
    | None -> assert_failure "Failed to fire transition"
    | Some fired_net ->

      assert_equal (Some 0) (marking_of_place p_in fired_net);
      assert_equal (Some 1) (marking_of_place p_out fired_net);

      assert_bool "Transition should be disabled after firing (not enough tokens for next fire)"
        (not (is_enabled t fired_net));
    end
)

let test_fire2 = "fire consumes multiple tokens from input" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 2 2
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 2)
              |>> add_arc (mk_incoming_arc t p_out 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->

    assert_equal (Some 2) (marking_of_place p_in net);
    assert_equal (Some 0) (marking_of_place p_out net);

    assert_bool "Transition should be enabled" 
      (is_enabled t net);

    begin match fire t net with
    | None -> assert_failure "Failed to fire transition"
    | Some fired_net ->

      assert_equal (Some 0) (marking_of_place p_in fired_net);
      assert_equal (Some 1) (marking_of_place p_out fired_net);

      assert_bool "Transition should be disabled after firing (not enough tokens for next fire)"
        (not (is_enabled t fired_net));
    end
)

let test_fire3 = "fire produces multiple tokens to output" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 1 1
              |> add_place p_out 2 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 1)
              |>> add_arc (mk_incoming_arc t p_out 2)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->

    assert_equal (Some 1) (marking_of_place p_in net);
    assert_equal (Some 0) (marking_of_place p_out net);

    assert_bool "Transition should be enabled" 
      (is_enabled t net);

    begin match fire t net with
    | None -> assert_failure "Failed to fire transition"
    | Some fired_net ->

      assert_equal (Some 0) (marking_of_place p_in fired_net);
      assert_equal (Some 2) (marking_of_place p_out fired_net);

      assert_bool "Transition should be disabled after firing (not enough tokens for next fire)"
        (not (is_enabled t fired_net));
    end
)

let test_fire4 = "fire with multiple input places join" >:: (fun _ ->
  let p_in_i = mk_place "P_in_i" in
  let p_in_ii = mk_place "P_in_ii" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in_i 1 1
              |> add_place p_in_ii 1 1
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in_i t 1)
              |>> add_arc (mk_outgoing_arc p_in_ii t 1)
              |>> add_arc (mk_incoming_arc t p_out 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->

    assert_equal (Some 1) (marking_of_place p_in_i net);
    assert_equal (Some 1) (marking_of_place p_in_ii net);
    assert_equal (Some 0) (marking_of_place p_out net);

    assert_bool "Transition should be enabled" 
      (is_enabled t net);

    begin match fire t net with
    | None -> assert_failure "Failed to fire transition"
    | Some fired_net ->

      assert_equal (Some 0) (marking_of_place p_in_i fired_net);
      assert_equal (Some 0) (marking_of_place p_in_ii fired_net);
      assert_equal (Some 1) (marking_of_place p_out fired_net);

      assert_bool "Transition should be disabled after firing (not enough tokens for next fire)"
        (not (is_enabled t fired_net));
    end
)

let test_fire5 = "fire with multiple output places fork" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out_i = mk_place "P_out_i" in
  let p_out_ii = mk_place "P_out_ii" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 1 1
              |> add_place p_out_i 1 0
              |> add_place p_out_ii 1 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 1)
              |>> add_arc (mk_incoming_arc t p_out_i 1)
              |>> add_arc (mk_incoming_arc t p_out_ii 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->

    assert_equal (Some 1) (marking_of_place p_in net);
    assert_equal (Some 0) (marking_of_place p_out_i net);
    assert_equal (Some 0) (marking_of_place p_out_ii net);

    assert_bool "Transition should be enabled" 
      (is_enabled t net);

    begin match fire t net with
    | None -> assert_failure "Failed to fire transition"
    | Some fired_net ->

      assert_equal (Some 0) (marking_of_place p_in fired_net);
      assert_equal (Some 1) (marking_of_place p_out_i fired_net);
      assert_equal (Some 1) (marking_of_place p_out_ii fired_net);

      assert_bool "Transition should be disabled after firing (not enough tokens for next fire)"
        (not (is_enabled t fired_net));
    end
)

let test_fire6 = "fire chained transitions with intermediate place" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_intermediate = mk_place "P_intermediate" in
  let p_out = mk_place "P_out" in
  let t1 = mk_transition "T1" in
  let t2 = mk_transition "T2" in

  let net_opt = empty 
              |> add_place p_in 1 1
              |> add_place p_intermediate 1 0
              |> add_place p_out 1 0
              |> add_transition t1
              |> add_transition t2
              |> add_arc (mk_outgoing_arc p_in t1 1)
              |>> add_arc (mk_incoming_arc t1 p_intermediate 1)
              |>> add_arc (mk_outgoing_arc p_intermediate t2 1)
              |>> add_arc (mk_incoming_arc t2 p_out 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->

    assert_equal (Some 1) (marking_of_place p_in net);
    assert_equal (Some 0) (marking_of_place p_intermediate net);
    assert_equal (Some 0) (marking_of_place p_out net);

    assert_bool "Transition T1 should be enabled" 
      (is_enabled t1 net);
    assert_bool "Transition T2 should not be enabled" 
      (not (is_enabled t2 net));

    begin match fire t1 net with
    | None -> assert_failure "Failed to fire transition"
    | Some fired_net ->

      assert_equal (Some 0) (marking_of_place p_in fired_net);
      assert_equal (Some 1) (marking_of_place p_intermediate fired_net);
      assert_equal (Some 0) (marking_of_place p_out fired_net);

      assert_bool "Transition should be disabled after firing (not enough tokens for next fire)"
        (not (is_enabled t1 fired_net));
      assert_bool "Transition T2 should be enabled" 
        (is_enabled t2 fired_net);
    end
)

let test_fire7 = "fire fails with insufficient input tokens" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 1 1  (* only 1 token present *)
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 2)
              |>> add_arc (mk_incoming_arc t p_out 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->
    assert_bool "Transition should NOT be enabled due to insufficient tokens"
      (not (is_enabled t net));
    
    assert_equal None (fire t net)
)

let test_fire8 = "fire fails with insufficient output capacity" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 1 1
              |> add_place p_out 2 1  (* capacity 2 but already 1 token, so only 1 slot left *)
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 1)
              |>> add_arc (mk_incoming_arc t p_out 2)
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->
    assert_bool "Transition should NOT be enabled due to insufficient output capacity"
      (not (is_enabled t net));
    
    assert_equal None (fire t net)
)

let test_fire9 = "fire transition multiple times in a row" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty
              |> add_place p_in 5 5
              |> add_place p_out 5 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 1)
              |>> add_arc (mk_incoming_arc t p_out 1)
              |>> fire t
              |>> fire t
              |>> fire t
  in
  match net_opt with
  | None -> assert_failure "Failed to build the petri net"
  | Some net ->
    assert_equal (Some 2) (marking_of_place p_in net);
    assert_equal (Some 3) (marking_of_place p_out net)
)

let test_fire10 = "fire transition that does not have arcs" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let t = mk_transition "T" in

  let net_opt = empty
              |> add_place p_in 5 5
              |> add_transition t
              |> fire t
  in
  match net_opt with
  | None -> ()
  | Some _ ->
    assert_failure "fire should fail due to no arcs"
)

let test_fire11 = "fire transition that does not have outgoing arcs" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 2 2
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_incoming_arc t p_out 1)
              |>> fire t
  in
  match net_opt with
  | None -> ()
  | Some _ ->
    assert_failure "fire should fail due to no outgoing arcs"
)

let test_fire12 = "fire transition that does not have incomming arcs" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 2 2
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 2)
              |>> fire t
  in
  match net_opt with
  | None -> ()
  | Some _ ->
    assert_failure "fire should fail due to no incomming arcs"
)


(**********************************************************************)
(*                         TESTING FIRE_ALL                           *)
(**********************************************************************)

let test_fire_all1 = "fire_all with empty transition list" >:: (fun _ ->
  let p = mk_place "P" in
  let t = mk_transition "T" in
  let net_opt = empty
                |> add_place p 1 1
                |> add_transition t
                |> add_arc (mk_outgoing_arc p t 1)
                |>> add_arc (mk_incoming_arc t p 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build net"
  | Some net ->
    match fire_all [] net with
    | None -> assert_failure "fire_all failed with empty transition list"
    | Some result_net ->
      assert_equal (marking_of_place p net) (marking_of_place p result_net);
)

let test_fire_all2 = "fire_all with single enabled transition" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in
  let net_opt = empty
                |> add_place p_in 1 1
                |> add_place p_out 1 0
                |> add_transition t
                |> add_arc (mk_outgoing_arc p_in t 1)
                |>> add_arc (mk_incoming_arc t p_out 1)
                |>> fire_all [t]
  in
  match net_opt with
    | None -> assert_failure "fire_all failed on a single transition"
    | Some net ->
      assert_equal (Some 0) (marking_of_place p_in net);
      assert_equal (Some 1) (marking_of_place p_out net)
)

let test_fire_all3 = "fire_all with chained transitions" >:: (fun _ ->
  let p1 = mk_place "P1" in
  let p2 = mk_place "P2" in
  let p3 = mk_place "P3" in
  let t1 = mk_transition "T1" in
  let t2 = mk_transition "T2" in
  let net_opt = empty
                |> add_place p1 1 1
                |> add_place p2 1 0
                |> add_place p3 1 0
                |> add_transition t1
                |> add_transition t2
                |> add_arc (mk_outgoing_arc p1 t1 1)
                |>> add_arc (mk_incoming_arc t1 p2 1)
                |>> add_arc (mk_outgoing_arc p2 t2 1)
                |>> add_arc (mk_incoming_arc t2 p3 1)
                |>> fire_all [t1; t2]
  in
  match net_opt with
    | None -> assert_failure  "fire_all failed for chained transitions"
    | Some result_net ->
      assert_equal (Some 0) (marking_of_place p1 result_net);
      assert_equal (Some 0) (marking_of_place p2 result_net);
      assert_equal (Some 1) (marking_of_place p3 result_net)
)

let test_fire_all4 = "fire_all fails part way" >:: (fun _ ->
  let p1 = mk_place "P1" in
  let p2 = mk_place "P2" in
  let t1 = mk_transition "T1" in
  let t2 = mk_transition "T2" in
  let net_opt = empty
                |> add_place p1 1 1
                |> add_place p2 1 0
                |> add_transition t1
                |> add_transition t2
                |> add_arc (mk_outgoing_arc p1 t1 1)
                |>> add_arc (mk_incoming_arc t1 p2 1)
                |>> fire_all [t1; t2]
  in
  match net_opt with
    | None -> ()  (* success, it should fail *)
    | Some _ -> assert_failure "fire_all should fail due to t2 not being enabled"
)

let test_fire_all5 = "fire_all transition that does not have outgoing arcs" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 2 2
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_incoming_arc t p_out 1)
              |>> fire_all [t]
  in
  match net_opt with
  | None -> ()
  | Some _ ->
    assert_failure "fire_all should fail due to no outgoing arcs"
)

let test_fire_all6 = "fire_all transition that does not have incomming arcs" >:: (fun _ ->
  let p_in = mk_place "P_in" in
  let p_out = mk_place "P_out" in
  let t = mk_transition "T" in

  let net_opt = empty 
              |> add_place p_in 2 2
              |> add_place p_out 1 0
              |> add_transition t
              |> add_arc (mk_outgoing_arc p_in t 2)
              |>> fire_all [t]
  in
  match net_opt with
  | None -> ()
  | Some _ ->
    assert_failure "fire_all should fail due to no incomming arcs"
)



(**********************************************************************)
(*                         TESTING IS_STUCK                           *)
(**********************************************************************)

let test_is_stuck1 = "empty net is stuck" >:: (fun _ ->
  let net = empty in
  assert_bool "Empty net should be stuck" (is_stuck net)
)

let test_is_stuck2 = "net with no transactions should be stuck" >:: (fun _ ->
  let p1 = mk_place "P" in

  let net = empty |> add_place p1 1 1 in
  assert_bool "Net should be stuck" (is_stuck net)
)

let test_is_stuck3 = "net with one place only, and enabled transition is not stuck" >:: (fun _ ->
  let p = mk_place "P" in
  let t = mk_transition "T" in

  let net = empty
              |> add_place p 3 1
              |> add_transition t in

  let net_opt = net
              |> add_arc (mk_outgoing_arc p t 1)
              |>> add_arc (mk_incoming_arc t p 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build net"
  | Some net ->
    assert_bool "Transition should be enabled" (is_enabled t net);
    assert_bool "Net should not be stuck" (not (is_stuck net))
)

let test_is_stuck4 = "net with one place only, and no enabled transitions is stuck" >:: (fun _ ->
  let p = mk_place "P" in
  let t = mk_transition "T" in

  let net = empty
              |> add_place p 1 1  (* insufficient tokens *)
              |> add_transition t in

  let net_opt = net
              |> add_arc (mk_outgoing_arc p t 2)
              |>> add_arc (mk_incoming_arc t p 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build net"
  | Some net ->
    assert_bool "Transition should NOT be enabled" (not (is_enabled t net));
    assert_bool "Net should be stuck" (is_stuck net)
)

let test_is_stuck5 = "net with some enabled transitions is not stuck" >:: (fun _ ->
  let p1 = mk_place "P1" in
  let p2 = mk_place "P2" in
  let p3 = mk_place "P3" in
  let t1 = mk_transition "T1" in
  let t2 = mk_transition "T2" in

  let net = empty
              |> add_place p1 1 1
              |> add_place p2 1 0
              |> add_place p3 1 0
              |> add_transition t1
              |> add_transition t2 in

  let net_opt = net
              |> add_arc (mk_outgoing_arc p1 t1 1)
              |>> add_arc (mk_incoming_arc t1 p2 1)
              |>> add_arc (mk_outgoing_arc p2 t2 1)
              |>> add_arc (mk_incoming_arc t2 p3 1)
  in
  match net_opt with
  | None -> assert_failure "Failed to build net"
  | Some net ->
    assert_bool "T1 should be enabled" (is_enabled t1 net);
    assert_bool "T2 should NOT be enabled" (not (is_enabled t2 net));
    assert_bool "Net should NOT be stuck" (not (is_stuck net))
)

let test_is_stuck6 = "net with one wrong transition and is not stuck" >:: (fun _ ->
  let p1 = mk_place "P1" in
  let p2 = mk_place "P2" in
  
  let t1 = mk_transition "T1" in
  let t2 = mk_transition "T2" in

  let net = empty
              |> add_place p1 2 2
              |> add_place p2 4 1
              |> add_transition t1
              |> add_transition t2 in
  let net_opt = net
              |> add_arc (mk_outgoing_arc p1 t1 2)
              |>> add_arc (mk_incoming_arc t1 p2 2)
              |>> add_arc (mk_outgoing_arc p2 t2 2)
  in
  match net_opt with
  | None -> assert_failure "Failed to build net"
  | Some net ->
    assert_bool "T1 should be enabled" (is_enabled t1 net);
    assert_bool "T2 should not be enabled, does not have incoming arc" (not (is_enabled t2 net));
    assert_bool "Net should not be stuck" (not (is_stuck net))
)

let test_is_stuck7 = "net with all transitions enable is not stuck" >:: (fun _ ->
  let p1 = mk_place "P1" in
  let p2 = mk_place "P2" in
  let p3 = mk_place "P3" in
  let t1 = mk_transition "T1" in
  let t2 = mk_transition "T2" in

  let net = empty
              |> add_place p1 2 2
              |> add_place p2 4 2
              |> add_place p3 2 0
              |> add_transition t1
              |> add_transition t2 in
  let net_opt = net
              |> add_arc (mk_outgoing_arc p1 t1 2)
              |>> add_arc (mk_incoming_arc t1 p2 2)
              |>> add_arc (mk_outgoing_arc p2 t2 2)
              |>> add_arc (mk_incoming_arc t2 p3 2)
  in
  match net_opt with
  | None -> assert_failure "Failed to build net"
  | Some net ->
    assert_bool "T1 should be enabled" (is_enabled t1 net);
    assert_bool "T2 should be enabled" (is_enabled t2 net);
    assert_bool "Net should not be stuck" (not (is_stuck net))
)

(**********************************************************************)
(*                          TESTING ALL                               *)
(**********************************************************************)

  let tests_fire = [
    test_fire1
    ; test_fire2
    ; test_fire3
    ; test_fire4
    ; test_fire5
    ; test_fire6
    ; test_fire7
    ; test_fire8
    ; test_fire9
    ; test_fire10
    ; test_fire11
    ; test_fire12
  ]

  let tests_is_stuck = [
    test_is_stuck1
    ; test_is_stuck2
    ; test_is_stuck3
    ; test_is_stuck4
    ; test_is_stuck5
    ; test_is_stuck6
    ; test_is_stuck7
  ]

  let tests_fire_all = [
    test_fire_all1;
    test_fire_all2;
    test_fire_all3;
    test_fire_all4;
    test_fire_all5;
    test_fire_all6;
  ]
  let tests = 
    canary_test :: test_empty
    :: (tests_fire @ tests_fire_all @ tests_is_stuck)

let test_suite_unit_tests_p3 = "Unit tests for P3" >::: tests;;