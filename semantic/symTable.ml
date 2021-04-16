open Symbol

(* symbol -> 'a への写像 *)
module Table = Map.Make (struct
  type t = symbol
  let compare (_, n1) (_, n2) = compare n1 n2
end)

type 'a table = 'a Table.t

let empty = Table.empty

let enter = Table.add

let lookup = Table.find_opt
(* lookup k t = try Some (Table.find k t) with Not_found -> None *)

let enter_list li tab= List.fold_left
  (fun tab (key , v) -> enter key v tab)
  tab
  li
