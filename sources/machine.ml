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

let one_char field str =
  if String.length str = 1 then str.[0]
  else raise (InvalidMachine (Printf.sprintf "'%s' must be a 1-char string, got %S" field str))

let contains_string (xs : string list) (x : string) = List.exists ((=) x) xs
let contains_char   (xs : char list)   (x : char)   = List.exists ((=) x) xs

let load_machine (file_path : string) : turing_machine =
  let json =
    try Yojson.Safe.from_file file_path with
    | Sys_error e         -> raise (InvalidMachine ("Cannot open file: " ^ e))
    | Yojson.Json_error e -> raise (InvalidMachine ("Invalid JSON: " ^ e))
  in

  let name           = json |> member "name"        |> to_string in
  let alphabet_raw   = json |> member "alphabet"    |> to_list   |> filter_string in
  let blank_raw      = json |> member "blank"       |> to_string in
  let states         = json |> member "states"      |> to_list   |> filter_string in
  let initial_state  = json |> member "initial"     |> to_string in
  let finals         = json |> member "finals"      |> to_list   |> filter_string in
  let transitions_by_state = json |> member "transitions" |> to_assoc in

  let alphabet =
    match alphabet_raw with
    | []    -> raise (InvalidMachine "Alphabet must be a non-empty array of 1-char strings")
    | chars -> List.map (one_char "alphabet") chars
  in
  let blank_symbol = one_char "blank" blank_raw in

  if not (contains_string states initial_state) then
    raise (InvalidMachine (Printf.sprintf "Initial state %S not in states" initial_state));
  List.iter (fun s ->
    if not (contains_string states s) then
      raise (InvalidMachine (Printf.sprintf "Final state %S not in states" s))
  ) finals;
  if not (contains_char alphabet blank_symbol) then
    raise (InvalidMachine (Printf.sprintf "Blank symbol %C not in alphabet" blank_symbol));

  let transition_table =
    List.fold_left
      (fun acc (current_state, state_rules_json) ->
         let state_rules = to_list state_rules_json in
         List.fold_left
           (fun acc rule_json ->
              let read_s   = rule_json |> member "read"     |> to_string in
              let write_s  = rule_json |> member "write"    |> to_string in
              let next_st  = rule_json |> member "to_state" |> to_string in
              let action_s = rule_json |> member "action"   |> to_string in
              let read_ch   = one_char "read" read_s   in
              let write_ch  = one_char "write" write_s in

              if not (contains_char alphabet read_ch)  then
                raise (InvalidMachine (Printf.sprintf "Read char %C not in alphabet" read_ch));
              if not (contains_char alphabet write_ch) then
                raise (InvalidMachine (Printf.sprintf "Write char %C not in alphabet" write_ch));
              if not (contains_string states next_st) then
                raise (InvalidMachine (Printf.sprintf "Next state %S not in states" next_st));

              let move_dir =
                match action_s with
                | "LEFT"  -> MoveLeft
                | "RIGHT" -> MoveRight
                | _ ->
                    raise (InvalidMachine (Printf.sprintf "Invalid action %S (expected LEFT|RIGHT)" action_s))
              in

              let key = (current_state, read_ch) in
              if TransitionMap.mem key acc then
                raise (InvalidMachine (Printf.sprintf "Duplicate transition for state=%S read=%C" current_state read_ch));

              TransitionMap.add key { write_char = write_ch; move_dir; next_state = next_st } acc
           )
           acc state_rules
      )
      TransitionMap.empty
      transitions_by_state
  in

  {
    name;
    alphabet;
    blank_symbol;
    states;
    initial_state;
    final_states = finals;
    transition_table;
  }

let find_transition (machine : turing_machine) ~(state:string) ~(read:char)
  : transition_rule option =
  TransitionMap.find_opt (state, read) machine.transition_table
