open Machine
open Tape

let move_to_string = function
  | MoveLeft  -> "LEFT"
  | MoveRight -> "RIGHT"

let char_list_to_string (chars : CharSet.t) : string =
  let buf = Buffer.create 32 in
  Buffer.add_char buf '[';
  CharSet.iter (fun c ->
    Buffer.add_string buf (Printf.sprintf "'%c'" c)
  ) chars;
  Buffer.add_char buf ']';
  Buffer.contents buf

let string_list_to_string (strs : StrSet.t) : string =
  let buf = Buffer.create 32 in
  Buffer.add_char buf '[';
  StrSet.iter (fun s ->
    Buffer.add_string buf (Printf.sprintf "%S" s)
  ) strs;
  Buffer.add_char buf ']';
  Buffer.contents buf

let print_machine (machine : turing_machine) : unit =
  let num_transitions = TransitionMap.cardinal machine.transition_table in
  Printf.printf "=== Machine: %s ===\n\n" machine.name;
  Printf.printf "Alphabet     : %s\n" (char_list_to_string machine.alphabet);
  Printf.printf "Blank symbol : '%c'\n" machine.blank;
  Printf.printf "States       : %s\n" (string_list_to_string machine.states);
  Printf.printf "Initial      : %s\n" machine.initial_state;
  Printf.printf "Finals       : %s\n" (string_list_to_string machine.final_states);
  Printf.printf "Transitions  : %d\n" num_transitions;
  TransitionMap.iter
    (fun (state, read_char) { write_char; move_dir; next_state } ->
      Printf.printf "  (%s, '%c') -> write '%c', move %s, next %s\n"
        state read_char write_char (move_to_string move_dir) next_state
    )
    machine.transition_table;
  print_endline "";
  print_endline "====================";
  flush stdout

let tape_to_str ~(tape : Tape.tape) ~(head_pos : int) ~(blank : char) ~(right_padding : int) : string =
  let low_bound =
    match Tape.min_index tape with
    | i -> min 0 i
    | exception _ -> 0
  in
  let high_data =
    match Tape.max_index tape with
    | i -> i
    | exception _ -> -1
  in
  let high_bound = max high_data (head_pos + right_padding) in
  if high_bound < low_bound then "[]" else
  let buf = Buffer.create (high_bound - low_bound + 3) in
  Buffer.add_char buf '[';
  for i = low_bound to high_bound do
    let ch = Tape.read tape i ~blank in
    let visible = if ch = blank then '.' else ch in
    if i = head_pos then (
      Buffer.add_char buf '<';
      Buffer.add_char buf visible;
      Buffer.add_char buf '>';
    ) else
      Buffer.add_char buf visible
  done;
  Buffer.add_char buf ']';
  Buffer.contents buf

let print_step
    (machine : turing_machine)
    ~(state : string)
    ~(head_pos : int)
    ~(tape : Tape.tape)
    ~(read_char : char)
    (transition : transition_rule)
  =
  let tape_str = tape_to_str ~tape ~head_pos ~blank:machine.blank ~right_padding:15 in
  Printf.printf "%s (%s, %c) -> (%s, %c, %s)\n%!"
    tape_str
    state
    read_char
    transition.next_state
    transition.write_char
    (move_to_string transition.move_dir)

let print_result ~halted ~state ~steps tape_output =
  let status = if halted then "HALT" else "BLOCKED" in
  Printf.printf "\n[%s] after %d step(s) in state %s\n" status steps state;
  Printf.printf "Final tape: %s\n%!" tape_output
