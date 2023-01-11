(* open Tree.Bagol *)
open OUnit2

module Data = Map.Make (String)

let tests = "interpreter test suite" >::: []

let _ = run_test_tt_main tests

