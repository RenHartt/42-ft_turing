let usage_msg =
  "usage: ft_turing [-h] jsonfile input\n\n\
  positional arguments:\n\
  jsonfile   json description of the machine\n\
  input      input of the machine\n\n\
  optional arguments:\n\
  -h, --help show this help message and exit"

let error_msg =
  "Error: invalid usage.\nUse --help for more information."

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [_; ("-h" | "--help")] ->
      print_endline usage_msg

  | [_; jsonfile; input] ->
      (try
         let machine = Machine.load_machine jsonfile in
         print_endline "Valid machine"
       with
       | Machine.InvalidMachine msg ->
           Printf.eprintf "Invalid machine: %s\n" msg
      )

  | _ ->
      print_endline error_msg
