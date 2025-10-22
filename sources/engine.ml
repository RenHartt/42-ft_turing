open Machine

type machine_state = {
  tape       : Tape.tape;
  head_pos   : int;
  state      : string;
  step_count : int;
}

let init_state (machine : Machine.turing_machine) (input_tape : string) : machine_state =
  let tape0 = Tape.make ~input:input_tape ~blank:machine.blank_symbol in
  { tape = tape0; head_pos = 0; state = machine.initial_state; step_count = 0 }

let step_once (machine : Machine.turing_machine) (ms : machine_state) : machine_state option =
  let current_char = Tape.read ms.tape ms.head_pos ~blank:machine.blank_symbol in
  match Machine.find_transition machine ~state:ms.state ~read:current_char with
  | None -> None
  | Some transition ->
      IO.print_step machine
        ~state:ms.state
        ~head_pos:ms.head_pos
        ~tape:ms.tape
        ~read_char:current_char
        transition;

      let tape_next =
        Tape.write ms.tape ms.head_pos transition.write_char ~blank:machine.blank_symbol
      in
      let head_next =
        match transition.move_dir with
        | MoveLeft  -> ms.head_pos - 1
        | MoveRight -> ms.head_pos + 1
      in
      let state_next = transition.next_state in
      Some { tape = tape_next; head_pos = head_next; state = state_next; step_count = ms.step_count + 1 }

let run (machine : Machine.turing_machine) (input_tape : string) : unit =
  let rec run_loop (ms : machine_state) : machine_state =
    if List.exists ((=) ms.state) machine.final_states then ms
    else
      match step_once machine ms with
      | None        -> ms
      | Some ms_next -> run_loop ms_next
  in
  let initial_ms = init_state machine input_tape in
  let final_ms   = run_loop initial_ms in
  let halted     = List.exists ((=) final_ms.state) machine.final_states in
  let tape_view  = Tape.to_string final_ms.tape ~blank:machine.blank_symbol in
  IO.print_result ~halted ~state:final_ms.state ~steps:final_ms.step_count tape_view
