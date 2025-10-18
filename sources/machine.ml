open Yojson.Safe.Util

type move_direction = MoveLeft | MoveRight

type transition_rule = {
  write_char : char;
  move_dir   : move_direction;
  next_state : string;
}

module TransitionKey = struct
  type t = string * char
  let compare (s1, c1) (s2, c2) =
    let k = Stdlib.compare s1 s2 in
    if k <> 0 then k else Stdlib.compare c1 c2
end

module TransitionMap = Map.Make(TransitionKey)

type turing_machine = {
  name             : string;
  alphabet         : char list;
  blank_symbol     : char;
  states           : string list;
  initial_state    : string;
  final_states     : string list;
  transition_table : transition_rule TransitionMap.t;
}

exception InvalidMachine of string

let extract_single_char field_name str =
  if String.length str = 1 then str.[0]
  else raise (InvalidMachine (Printf.sprintf "'%s' must be a 1-char string, got %S" field_name str))

let string_in_list lst s = List.exists ((=) s) lst
let char_in_list   lst c = List.exists ((=) c) lst

let load_machine (file_path : string) : turing_machine =
  let json_data =
    try Yojson.Safe.from_file file_path with
    | Sys_error e -> raise (InvalidMachine ("Cannot open file: " ^ e))
    | Yojson.Json_error e -> raise (InvalidMachine ("Invalid JSON: " ^ e))
  in

  let machine_name  = json_data |> member "name"        |> to_string in
  let alphabet_json = json_data |> member "alphabet"    |> to_list   |> filter_string in
  let blank_str     = json_data |> member "blank"       |> to_string in
  let state_list    = json_data |> member "states"      |> to_list   |> filter_string in
  let initial_state = json_data |> member "initial"     |> to_string in
  let final_list    = json_data |> member "finals"      |> to_list   |> filter_string in
  let transitions   = json_data |> member "transitions" |> to_assoc  in

  let alphabet =
    match alphabet_json with
    | [] -> raise (InvalidMachine "Alphabet must be a non-empty array of 1-char strings")
    | chars -> List.map (extract_single_char "alphabet") chars
  in
  let blank_symbol = extract_single_char "blank" blank_str in

  if not (string_in_list state_list initial_state) then
    raise (InvalidMachine (Printf.sprintf "Initial state %S not in states" initial_state));
  List.iter (fun s ->
    if not (string_in_list state_list s) then
      raise (InvalidMachine (Printf.sprintf "Final state %S not in states" s))
  ) final_list;
  if not (char_in_list alphabet blank_symbol) then
    raise (InvalidMachine (Printf.sprintf "Blank symbol %C not in alphabet" blank_symbol));

  let transition_table =
    List.fold_left
      (fun acc (current_state, transitions_json) ->
         let transitions_list = to_list transitions_json in
         List.fold_left
           (fun acc transition_json ->
              let read_str   = transition_json |> member "read"     |> to_string in
              let write_str  = transition_json |> member "write"    |> to_string in
              let next_state = transition_json |> member "to_state" |> to_string in
              let action_str = transition_json |> member "action"   |> to_string in

              let read_char  = extract_single_char "read" read_str   in
              let write_char = extract_single_char "write" write_str in

              if not (char_in_list alphabet read_char)  then raise (InvalidMachine (Printf.sprintf "Read char %C not in alphabet" read_char));
              if not (char_in_list alphabet write_char) then raise (InvalidMachine (Printf.sprintf "Write char %C not in alphabet" write_char));
              if not (string_in_list state_list next_state) then raise (InvalidMachine (Printf.sprintf "Next state %S not in states" next_state));

              let move_dir =
                match action_str with
                | "LEFT"  -> MoveLeft
                | "RIGHT" -> MoveRight
                | _ -> raise (InvalidMachine (Printf.sprintf "Invalid action %S (expected LEFT|RIGHT)" action_str))
              in

              let key = (current_state, read_char) in
              if TransitionMap.mem key acc then
                raise (InvalidMachine (Printf.sprintf "Duplicate transition for state=%S read=%C" current_state read_char));

              TransitionMap.add key { write_char; move_dir; next_state } acc
           )
           acc transitions_list
      )
      TransitionMap.empty
      transitions
  in

  { name = machine_name;
    alphabet;
    blank_symbol;
    states = state_list;
    initial_state;
    final_states = final_list;
    transition_table }

let find_transition (machine : turing_machine) ~(state:string) ~(read:char)
  : transition_rule option =
  TransitionMap.find_opt (state, read) machine.transition_table
