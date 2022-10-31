(* insrert at specific index *)
let rec insert_at var i arr = match arr with
  | [] -> []
  | h::t -> if i = 0 then var::h::t 
            else if List.length arr = i then h::t@[var] 
            else h::insert_at var (i-1) t;;


(* insrert None at specific index *)
let insert_none_at i arr = insert_at "None" i arr;;