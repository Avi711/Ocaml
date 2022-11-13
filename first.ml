(* insrert at specific index *)
let rec insert_at var i arr = match arr with
  | [] -> []
  | h::t -> if i = 0 then var::h::t 
            else if List.length arr = i then h::t@[var] 
            else h::insert_at var (i-1) t;;


(* insrert None at specific index *)
let insert_none_at i arr = insert_at "None" i arr;;



type 'a binary_tree = 
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;


let rec add_to_search_tree t x = match t with
    | Empty -> Node(x,Empty,Empty)
    | Node(i,l,r) -> 
              if(x < i) then Node(i,add_to_search_tree l x, r)
              else Node(i,l,add_to_search_tree r x);;


type bool_expr = 
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;


let rec eval a b a_val b_val exp = match exp with
    | Var(x) -> if(x = a) then a_val else b_val
    | Not(x) -> not (eval a b a_val b_val x)
    | And(l,r) -> eval a b a_val b_val l && eval a b a_val b_val r
    | Or (l,r) -> eval a b a_val b_val l || eval a b a_val b_val r;;



let rec eval_2 list exp = match exp with
    | Var(x) -> get_val list x
    | Not(x) -> not (eval_2 list x)
    | And(l,r) -> eval_2 list l && eval_2 list r
    | Or (l,r) -> eval_2 list l || eval_2 list r;;


let rec get_val list v = match list with
    | [] -> false
    | (x,y) :: t -> if x = v then y else get_val t v;;


let table_two a b exp = 
    [(true, true, eval a b true true exp);
     (true, false, eval a b true false exp);
     (false, true, eval a b false true exp);
     (false, false, eval a b false false exp)];;

  
(* size should be the number of rows / 2 *)
let rec table_help list r size = match list with
    | [] -> []
    | h :: t -> if (r < size) then (h, true) :: table_help t r (size / 2) else (h, false) :: table_help t (r mod size) (size / 2);;


let rec table_help2 list n size exp =
                                 if (n = 0) then [(table_help list 0 size,eval_2 (table_help list n size) exp)]
                                 else (table_help2 list (n - 1) size exp) @ [((table_help list n size),eval_2 (table_help list n size) exp)];;
let rec power a x = 
                if x=0 then 1
                else a * power a (x-1);;


let table list exp =
                let size =  power 2 (List.length list) in
                table_help2 list (size - 1) (size / 2) exp;;
