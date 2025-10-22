module IntMap = Map.Make(Int)

type tape = char IntMap.t

let min_index (t : tape) =
  match IntMap.min_binding_opt t with
  | None -> 0
  | Some (k, _) -> k

let max_index (t : tape) =
  match IntMap.max_binding_opt t with
  | None -> -1
  | Some (k, _) -> k

let make ~input ~blank =
  let rec fill i acc =
    if i >= String.length input then acc
    else fill (i + 1) (IntMap.add i input.[i] acc)
  in
  fill 0 IntMap.empty

let read (t : tape) (pos : int) ~blank =
  match IntMap.find_opt pos t with
  | Some c -> c
  | None -> blank

let write (t : tape) (pos : int) (ch : char) ~blank =
  if ch = blank then IntMap.remove pos t
  else IntMap.add pos ch t

let to_string (t : tape) ~blank =
  match (IntMap.min_binding_opt t, IntMap.max_binding_opt t) with
  | None, _ | _, None -> ""
  | Some (lo, _), Some (hi, _) ->
      let buffer = Bytes.create (hi - lo + 1) in
      for i = lo to hi do
        Bytes.set buffer (i - lo) (read t i ~blank)
      done;
      Bytes.unsafe_to_string buffer
