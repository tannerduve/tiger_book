
type symbol = string * int 

  module H = Hashtbl

  exception Symbol
  let nextsym = ref 0 
  let sizeHint = 128
  let hashtable : (string, int) H.t = Hashtbl.create sizeHint
  let find_key tbl k = 
    match Hashtbl.find_opt tbl k with 
    | Some v -> Some v 
    | _ -> raise Symbol

  let symbol name =
    match H.find_opt hashtable name with 
    | Some v -> (name, v)
    | None -> 
        let i = !nextsym in 
          incr nextsym;
          H.add hashtable name i;
        (name, i)

  let name (s, _) = s

  module Table = Map.Make (struct
    type t = symbol
    let compare (_, n1) (_, n2) = Int.compare n1 n2
    end
  )

  type 'a table = 'a Table.t

  let empty = Table.empty

  let add = Table.add
  let look = Table.find_opt

module type Symbol = sig 
  type symbol

  val symbol : string -> symbol 
  val name : symbol -> string 

  type 'a table 
  val empty : 'a table 
  val add : symbol -> 'a -> 'a table -> 'a table
  val look : symbol -> 'a table -> 'a option

end
