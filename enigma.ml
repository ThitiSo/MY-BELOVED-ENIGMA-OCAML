type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}

let index _c = Char.code _c  -  65

let map_r_to_l wiring top_letter input_pos =
  let offset = index top_letter in
  let internal_pos = (input_pos + offset) mod 26 in
  let wired_char = String.get wiring internal_pos in
  let wired_pos = index wired_char in
  (wired_pos - offset + 26) mod 26

let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  let internal_pos = (input_pos + offset) mod 26 in
  let target_char = Char.chr (internal_pos + Char.code 'A') in

  let rec find_pos i =
    if String.get wiring i = target_char then i
    else find_pos (i + 1)
  in

  let wired_pos = find_pos 0 in
  (wired_pos - offset + 26) mod 26

let map_refl wiring input_pos =
  let reflected_char = String.get wiring input_pos in
  index reflected_char

let rec map_plug plugboard c =
  match plugboard with
  | [] -> c
  | (a, b) :: rest ->
      if c = a then b
      else if c = b then a
      else map_plug rest c

let cipher_char _config _c =
  failwith "Unimplemented"

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
