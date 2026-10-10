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

let cipher_char config c =
  let plugged_char = map_plug config.plugboard c in
  let input_pos = index plugged_char in

  let rec r_to_l rotors pos =
    match rotors with
    | [] -> pos
    | r :: rest ->
        r_to_l rest (map_r_to_l r.rotor.wiring r.top_letter pos)
  in

  let rec l_to_r rotors pos =
    match rotors with
    | [] -> pos
    | r :: rest ->
        l_to_r rest (map_l_to_r r.rotor.wiring r.top_letter pos)
  in

  let after_rotors =
    r_to_l (List.rev config.rotors) input_pos
  in

  let after_reflector =
    map_refl config.refl after_rotors
  in

  let after_reverse_rotors =
    l_to_r config.rotors after_reflector
  in

  let output_char =
    Char.chr (after_reverse_rotors + Char.code 'A')
  in

  map_plug config.plugboard output_char

let step config =
  let next_letter c =
    if c = 'Z' then 'A'
    else Char.chr (Char.code c + 1)
  in

  let step_rotor r =
    { r with top_letter = next_letter r.top_letter }
  in

  let rec step_rotors is_leftmost rotors =
    match rotors with
    | [] -> []

    (* rightmost rotor always steps *)
    | [r] ->
        [step_rotor r]

    | r :: ((next :: _) as rest) ->
        let at_turnover =
          (not is_leftmost)
          && r.top_letter = r.rotor.turnover
        in

        let right_at_turnover =
          next.top_letter = next.rotor.turnover
        in

        let new_r =
          if at_turnover || right_at_turnover then
            step_rotor r
          else
            r
        in

        new_r :: step_rotors false rest
  in

  { config with rotors = step_rotors true config.rotors }

let cipher config text =
  let rec aux config i result =
    if i = String.length text then
      result
    else
      let new_config = step config in
      let encrypted_char =
        cipher_char new_config (String.get text i)
      in
      aux new_config (i + 1) (result ^ String.make 1 encrypted_char)
  in
  aux config 0 ""

let hours_worked = 12
