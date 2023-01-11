open OUnit2
open Tree.Scanner
open Tree.Token

let file_test (name : string) (filename : string) : test list = 
  let file_channel = open_in ("data/scanning/" ^ filename ^ ".lox") in
  let code = In_channel.input_all file_channel in 
  let tokens = Scanner.scanTokens (String.to_seq code) in
  code |> String.split_on_char '\n'
    |> List.filter_map (fun s -> 
      if (String.length s >= 11 && String.sub s 0 11 = "// expect: ") then
      Some (String.sub s 11 (String.length s - 11)) else None)
    |> List.map2 (fun s t -> 
      name >:: fun _ -> 
        assert_equal s t ~printer:(fun x -> x)) 
        (List.map (fun t -> string_of_token t) tokens) 

let tests = "file test suite" >:::
  (["identifiers"; "keywords"; "numbers"; "punctuators"; "strings"; "whitespace" ] 
  |> List.map (fun t -> file_test t t) |> List.flatten)

let _ = run_test_tt_main tests 