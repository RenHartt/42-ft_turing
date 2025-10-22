open Machine

let string_of_move = function
  | MoveLeft  -> "LEFT"
  | MoveRight -> "RIGHT"

let string_of_char_list (lst : char list) : string =
  let b = Buffer.create 32 in
  Buffer.add_char b '[';
  List.iteri (fun i c ->
    if i > 0 then Buffer.add_string b ", ";
    Buffer.add_string b (Printf.sprintf "'%c'" c)
  ) lst;
  Buffer.add_char b ']';
  Buffer.contents b

let string_of_string_list (lst : string list) : string =
  let b = Buffer.create 32 in
  Buffer.add_char b '[';
  List.iteri (fun i s ->
    if i > 0 then Buffer.add_string b ", ";
    Buffer.add_string b (Printf.sprintf "%S" s)
  ) lst;
  Buffer.add_char b ']';
  Buffer.contents b

let print_machine_info (m : turing_machine) : unit =
  let tr_count = TransitionMap.cardinal m.transition_table in
  Printf.printf "=== Machine: %s ===\n\n" m.name;
  Printf.printf "Alphabet     : %s\n" (string_of_char_list m.alphabet);
  Printf.printf "Blank symbol : '%c'\n" m.blank_symbol;
  Printf.printf "States       : %s\n" (string_of_string_list m.states);
  Printf.printf "Initial      : %s\n" m.initial_state;
  Printf.printf "Finals       : %s\n" (string_of_string_list m.final_states);
  Printf.printf "Transitions  : %d\n" tr_count;
  TransitionMap.iter
    (fun (state, read_ch) { write_char; move_dir; next_state } ->
      Printf.printf "  (%s, '%c') → write '%c', move %s, next %s\n"
        state read_ch write_char (string_of_move move_dir) next_state
    )
    m.transition_table;
  print_endline "";
  print_endline "====================";
  flush stdout

let print_engine_state ~step ~state ~head tape_str =
  Printf.printf "[Step %d] State: %s | Head: %d | Tape: %s\n"
    step state head tape_str;
  flush stdout

let print_result ~halted ~state ~steps tape_str =
  let status = if halted then "HALT" else "BLOCKED" in
  Printf.printf "\n[%s] after %d step(s) in state %s\n" status steps state;
  Printf.printf "Final tape: %s\n%!" tape_str
