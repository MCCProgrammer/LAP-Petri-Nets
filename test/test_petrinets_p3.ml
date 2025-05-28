open OUnit2
open Unit_tests_p3
open Coffeemachine
open Buildfire

let _ = run_test_tt_main test_suite_unit_tests_p3
              
let _ = run_test_tt_main test_suite_buildfire

let _ = run_test_tt_main test_suite_coffeemachine

