[@@@ocaml.warning "-32-34-37"]

type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm 
         | AssignStm of id * exp 
         | PrintStm of exp list 
     and exp = IdExp of id 
             | NumExp of int 
             | OpExp of exp * binop * exp 
             | EseqExp of stm * exp

(* Chapter 1 Programming Exercise 1:
  Write an OCaml function maxargs : stm -> int that tells the max num of arguments of any print within any
  subexpression of a given statement
*)

let rec maxargs (s : stm) : int = 
  match s with 
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (_, e) -> maxargsexp e
  | PrintStm l -> max (List.fold_left (fun acc elem -> max acc (maxargsexp elem)) 0 l) (List.length l)
and maxargsexp (e : exp) : int = 
  match e with 
  | OpExp (e1, _, e2) -> max (maxargsexp e1) (maxargsexp e2)
  | EseqExp (s, e) -> max (maxargs s) (maxargsexp e)
  | _ -> 0

(* Chapter 1 Programming Exercise 2:
  Write an OCaml function interp : stm -> unit that "interprets" a program in this language.
  To write in FP style, maintain a list of variable,integer pairs, and produce a new version
  at each AssignStm
*)

type table = (id * int) list

let update (l : 'a list) (s : 'a) : 'a list =
  match l with
  | [] -> [s]
  | hd :: tl -> s :: hd :: tl 

let rec lookup (t : table) (i : id) : int =
  match t with
  | [] -> failwith ("unbound id: " ^ i)
  | (k, v) :: tl -> if k = i then v else lookup tl i

let rec interpStm (s : stm) (t : table) : table =
  match s with
  | CompoundStm (s1, s2) -> 
      let t2 = interpStm s1 t in
      interpStm s2 t2
  | AssignStm (i, e) -> 
      let (n , t2) = interpExp e t in
      update t2 (i, n)
  | PrintStm l ->
      let t' =
        List.fold_left (fun acc e ->
          let (v, acc') = interpExp e acc in
          Printf.printf "%d " v; 
          acc'
        ) t l
      in
      print_newline ();
      t'

and interpExp (e : exp) (t : table) : int * table =
  match e with 
  | IdExp i -> (lookup t i, t)
  | NumExp n -> (n, t)
  | OpExp (e1, op, e2) -> 
      let v1, t1 = interpExp e1 t in
      let v2, t2 = interpExp e2 t1 in
      let r = match op with
        | Plus -> v1 + v2
        | Minus -> v1 - v2
        | Times -> v1 * v2
        | Div -> v1 / v2
      in
      (r, t2)
  | EseqExp (s1, e1) -> 
      let t2 = interpStm s1 t in 
      interpExp e1 t2

let prog =
  CompoundStm (
    AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
    CompoundStm (
      AssignStm ("b",
        EseqExp (
          PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
          OpExp (NumExp 1, Times, IdExp "a")
        )
      ),
      PrintStm [ IdExp "b" ]
    )
  )

(* Exercises 
1.1. This program implements persistent functional BSTs, such that if tree₂ = insert x tree₁,
then tree₁ is available for lookups while tree₂ can be used
*)

(* 1.1a. Implement a `member` function which returns `true` if an item is found, else `false` *)
type key = string 
type tree = LEAF | TREE of tree * key * tree 

type empty = EmptyLeaf

let rec insert (k : key) (t : tree) : tree =
  match t with 
  | LEAF -> TREE (LEAF, k, LEAF)
  | TREE (l, k', r) -> 
      if k < k' then TREE (insert k l, k', r)
      else if k > k' then TREE (l, k', insert k r)
      else TREE (l, k, r)

(* 1.1a. Implement a `member` function which returns `true` if an item is found, else `false` *)
let rec member (k : key) (t : tree) : bool = 
  match t with 
  | LEAF -> false 
  | TREE (l, k', r) -> 
      if k < k' then member k l 
      else if k > k' then member k r 
      else true

(* 1.1b Extend the program to include the mapping of keys to bindings*)

type 'a ptree = PLeaf | PNode of 'a ptree * (key * 'a) * 'a ptree 

let rec pinsert (t : 'a ptree) (k : key) (v : 'a) : 'a ptree =
  match t with 
  | PLeaf -> PNode (PLeaf, (k, v), PLeaf)
  | PNode (l, (x, v'), r) -> 
      if k < x then PNode (pinsert l k v, (x, v'), r)
      else if k > x then PNode (l, (x, v'), pinsert r k v)
      else PNode (l, (k, v), r)

let rec pmember (t : 'a ptree) (k : key) : 'a =
  match t with 
  | PLeaf -> failwith ("unbound id: " ^ k)
  | PNode (l, (x, v'), r) -> 
      if k < x then pmember l k 
      else if k > x then pmember r k 
      else v'
