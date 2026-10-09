open OUnit2
open Enigma
let rotor_I =
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

let rotor_III =
  "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let identity =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let reflector_B =
  "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let rotor_I_record =
  { wiring = rotor_I; turnover = 'Q' }

let rotor_II_record =
  { wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE"; turnover = 'E' }

let rotor_III_record =
  { wiring = rotor_III; turnover = 'V' }

let identity_config = {
  refl = identity;
  rotors = [];
  plugboard = [];
}

let historical_config = {
  refl = reflector_B;
  rotors = [
    { rotor = rotor_I_record; top_letter = 'A' };
    { rotor = rotor_II_record; top_letter = 'A' };
    { rotor = rotor_III_record; top_letter = 'A' }
  ];
  plugboard = [];
}


let rotor_I_record =
  {
    wiring = rotor_I;
    turnover = 'Q';
  }

let rotor_II_record =
  {
    wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE";
    turnover = 'E';
  }

let rotor_III_record =
  {
    wiring = rotor_III;
    turnover = 'V';
  }

let step_config a b c =
  {
    refl = reflector_B;
    rotors = [
      { rotor = rotor_III_record; top_letter = a };
      { rotor = rotor_II_record; top_letter = b };
      { rotor = rotor_I_record; top_letter = c };
    ];
    plugboard = [];
  }

  
let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let map_r_to_l_test name wiring top_letter input expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top_letter input)

let map_l_to_r_test name wiring top_letter input expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top_letter input)

let map_refl_test name wiring input expected =
  name >:: fun _ ->
    assert_equal expected (map_refl wiring input)

let map_plug_test name plugboard input expected =
  name >:: fun _ ->
    assert_equal expected (map_plug plugboard input)

let cipher_char_test name config input expected =
  name >:: fun _ ->
    assert_equal expected (cipher_char config input)

let step_test name config expected =
  name >:: fun _ ->
    let stepped = step config in
    let positions =
      List.map (fun r -> r.top_letter) stepped.rotors
    in
    assert_equal expected positions
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

    (* Reflector*)
    map_refl_test "identity reflector maps 0 to 0"
      identity 0 0;

    map_refl_test "identity reflector maps 1 to 1"
      identity 1 1;

    map_refl_test "identity reflector maps 25 to 25"
      identity 25 25;

    map_refl_test "reflector B maps 0 to 24"
      reflector_B 0 24;

    map_refl_test "reflector B maps 24 to 0"
      reflector_B 24 0;


    (* Plugboard *)

    map_plug_test "empty plugboard leaves A unchanged"
      [] 'A' 'A';

    map_plug_test "AZ maps A to Z"
      [('A', 'Z')] 'A' 'Z';

    map_plug_test "AZ maps Z to A"
      [('A', 'Z')] 'Z' 'A';

    map_plug_test "multiple pairs map X to Y"
      [('A', 'Z'); ('X', 'Y')] 'X' 'Y';

    map_plug_test "unconnected letter stays unchanged"
      [('A', 'Z'); ('X', 'Y')] 'M' 'M';
    
        (* cipher_char *)

    cipher_char_test "identity cipher maps A to A"
      identity_config 'A' 'A';

    cipher_char_test "identity cipher maps M to M"
      identity_config 'M' 'M';

    cipher_char_test "identity cipher maps Z to Z"
      identity_config 'Z' 'Z';

    cipher_char_test "historical config maps G to P"
      historical_config 'G' 'P';

    cipher_char_test "historical config maps A to U"
      historical_config 'A' 'U';


    (* step *)

    step_test "KDO steps to KDP"
      (step_config 'K' 'D' 'O')
      ['K'; 'D'; 'P'];

    step_test "KDP steps to KDQ"
      (step_config 'K' 'D' 'P')
      ['K'; 'D'; 'Q'];

    step_test "KDQ turnover steps to KER"
      (step_config 'K' 'D' 'Q')
      ['K'; 'E'; 'R'];

    step_test "KER double stepping gives LFS"
      (step_config 'K' 'E' 'R')
      ['L'; 'F'; 'S'];

    step_test "VDQ turnover steps to VER"
      (step_config 'V' 'D' 'Q')
      ['V'; 'E'; 'R'];
  ]

let () = run_test_tt_main suite
