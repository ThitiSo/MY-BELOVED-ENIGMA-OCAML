open OUnit2
open Enigma
let rotor_I =
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

let rotor_III =
  "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let identity =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let map_r_to_l_test name wiring top_letter input expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top_letter input)

let map_l_to_r_test name wiring top_letter input expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top_letter input)
let suite =
  "Enigma test suite" >::: [
    (* index *)
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;
    index_test "index of B is 1" 'B' 1;
    index_test "index of M is 12" 'M' 12;
    index_test "index of Y is 24" 'Y' 24;

      (* map_r_to_l *)
    map_r_to_l_test
      "r to l identity A 0"
      identity 'A' 0 0;

    map_r_to_l_test
      "r to l identity A 25"
      identity 'A' 25 25;

    map_r_to_l_test
      "r to l rotor I A 0"
      rotor_I 'A' 0 4;

    map_r_to_l_test
      "r to l rotor I B 0"
      rotor_I 'B' 0 9;

    map_r_to_l_test
      "r to l rotor III O 14"
      rotor_III 'O' 14 17;


    (* map_l_to_r *)
    map_l_to_r_test
      "l to r identity A 0"
      identity 'A' 0 0;

    map_l_to_r_test
      "l to r identity A 25"
      identity 'A' 25 25;

    map_l_to_r_test
      "l to r rotor I A 0"
      rotor_I 'A' 0 20;

    map_l_to_r_test
      "l to r rotor I F 10"
      rotor_I 'F' 10 14;

    map_l_to_r_test
      "l to r rotor I A 4"
      rotor_I 'A' 4 0;


    
  ]

let () = run_test_tt_main suite
