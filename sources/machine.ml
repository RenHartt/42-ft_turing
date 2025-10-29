open Yojson.Safe.Util

type move_direction = MoveLeft | MoveRight

type transition_rule = {
  write_char : char;
  move_dir : move_direction;
  next_state : string;
}

module TransitionKey = struct
  type t = string * char
  let compare (s1, c1) (s2, c2) =
    let k = Stdlib.compare s1 s2 in
    if k <> 0 then k else Stdlib.compare c1 c2
end

module TransitionMap = Map.Make(TransitionKey)
module CharSet = Set.Make(Char)
module StrSet = Set.Make(String)

type turing_machine = {
  name : string;
  alphabet : CharSet.t;
  blank : char;
  states : StrSet.t;
  initial_state : string;
  final_states : StrSet.t;
  transition_table : transition_rule TransitionMap.t;
}

exception InvalidMachine of string

let one_char field str =
  if String.length str = 1 then str.[0]
  else raise (InvalidMachine (Printf.sprintf "'%s' must be a 1-char string, got %S" field str))

let one_char_list str =
  if String.length str = 1 then str.[0]
  else raise (InvalidMachine (Printf.sprintf "Expected single character string, got %S" str))

let parse_move (s : string) : move_direction =
  match s with
  | "LEFT" -> MoveLeft
  | "RIGHT" -> MoveRight
  | _ ->
      raise (InvalidMachine (Printf.sprintf "Invalid action %S (expected LEFT|RIGHT)" s))

let check_states 
    ~(states : StrSet.t)
    ~(initial_state : string)
    ~(finals : StrSet.t) =
  if StrSet.is_empty states then
    raise (InvalidMachine "States cannot be empty");
  if StrSet.is_empty finals then
    raise (InvalidMachine "There must be a final state");
  if not (StrSet.mem initial_state states) then
    raise (InvalidMachine (Printf.sprintf "Initial state %S not in states" initial_state));
  StrSet.iter (fun s ->
    if not (StrSet.mem s states) then
      raise (InvalidMachine (Printf.sprintf "Final state %S not in states" s))
  ) finals

let check_alphabet
    ~(alphabet : CharSet.t)
    ~(blank : char)
  =
  if CharSet.is_empty alphabet then
    raise (InvalidMachine "Alphabet cannot be empty");
  if not (CharSet.mem blank alphabet) then
    raise (InvalidMachine (Printf.sprintf "Blank symbol %C not in alphabet" blank))

let check_machine_validity
    ~(states : StrSet.t)
    ~(alphabet : CharSet.t)
    ~(initial_state : string)
    ~(finals : StrSet.t)
    ~(blank : char)
  =
  check_states ~states ~initial_state ~finals;
  check_alphabet ~alphabet ~blank

let build_rule
    ~(alphabet : CharSet.t)
    ~(states : StrSet.t)
    ~(current_state : string)
    (rule_json : Yojson.Safe.t)
  : ((string * char) * transition_rule) =
  let read_s = rule_json |> member "read" |> to_string in
  let write_s = rule_json |> member "write" |> to_string in
  let next_st = rule_json |> member "to_state" |> to_string in
  let action_s = rule_json |> member "action" |> to_string in
  let read_ch = one_char "read" read_s in
  let write_ch = one_char "write" write_s in

  if not (CharSet.mem read_ch alphabet)  then
    raise (InvalidMachine (Printf.sprintf "Read char %C not in alphabet" read_ch));
  if not (CharSet.mem write_ch alphabet) then
    raise (InvalidMachine (Printf.sprintf "Write char %C not in alphabet" write_ch));
  if not (StrSet.mem next_st states)  then
    raise (InvalidMachine (Printf.sprintf "Next state %S not in states" next_st));

  let move_dir = parse_move action_s in
  let key = (current_state, read_ch) in
  let rule = { write_char = write_ch; move_dir; next_state = next_st } in
  (key, rule)

let add_rule
    (tbl : transition_rule TransitionMap.t)
    (key_rule : (string * char) * transition_rule)
  : transition_rule TransitionMap.t =
  let (key, rule) = key_rule in
  if TransitionMap.mem key tbl then
    let (state, read_ch) = key in
    raise (InvalidMachine (Printf.sprintf
      "Duplicate transition for state %S and read char %C"
      state read_ch))
  else
    TransitionMap.add key rule tbl

let build_state_transitions
    ~(alphabet : CharSet.t)
    ~(states : StrSet.t)
    (tbl : transition_rule TransitionMap.t)
    ((current_state, rules_json) : string * Yojson.Safe.t)
  : transition_rule TransitionMap.t =
  rules_json
  |> to_list
  |> List.fold_left
       (fun acc rule_json ->
          build_rule ~alphabet ~states ~current_state rule_json
          |> add_rule acc)
       tbl

let load_machine (file_path : string) : turing_machine =
  let json =
    try Yojson.Safe.from_file file_path with
    | Sys_error e -> raise (InvalidMachine ("Cannot open file: " ^ e))
    | Yojson.Json_error e -> raise (InvalidMachine ("Invalid JSON: " ^ e))
  in

  let name = json |> member "name" |> to_string in
  let alphabet = json |> member "alphabet" |> to_list |> filter_string |> List.map one_char_list |> CharSet.of_list in (* probleme ici *)
  let blank = json |> member "blank" |> to_string |> one_char "blank" in
  let states = json |> member "states" |> to_list |> filter_string |> StrSet.of_list in
  let initial_state = json |> member "initial" |> to_string in
  let finals = json |> member "finals" |> to_list |> filter_string |> StrSet.of_list in
  let transitions = json |> member "transitions" |> to_assoc in

  check_machine_validity 
      ~states
      ~alphabet
      ~initial_state
      ~finals
      ~blank;

  let transition_table =
    List.fold_left
      (build_state_transitions ~alphabet ~states)
      TransitionMap.empty
      transitions
  in

  {
    name;
    alphabet;
    blank;
    states;
    initial_state;
    final_states = finals;
    transition_table;
  }

let find_transition (machine : turing_machine) 
    ~(state:string)
    ~(read:char)
  : transition_rule option =
  TransitionMap.find_opt (state, read) machine.transition_table
