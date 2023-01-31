(* membro : 'a list -> 'a -> bool *)
let rec membro = function 
  [] -> (function m -> false)
  |h::t -> (function m -> if m=h then true else membro t m);;
  

(* conta : 'a -> 'a list -> int *)
let rec conta x = function
  [] -> 0
  | a::b -> if x=a then 1+conta x b else conta x b;;


(* append : 'a list -> 'a list -> 'a list *)
let rec append = function [] -> (function x -> x)
  | h::t -> (function x -> h::append t x);;


(* soma_lista : int list -> int *)
let rec soma_lista = function
  [] -> 0
  | h::t -> h+soma_lista t;;


(* remove : 'a -> 'a list -> 'a list *)
let rec remove x = function
  [] -> []
  |h::t -> if x=h then remove x t else h::remove x t;;  


let rec contagem = function
  | [] -> []
  | h::t -> [h;1+conta h t]::contagem (remove h t);;


(*------------------------------------Funções extra----------------------------------------------*)

(* 'a -> 'a list -> bool*)
let rec membro2 x = function
  [] -> false
  |h::t -> if x=h then true else membro2 x t;;



(*------------------------------------Funções extra utilizando match----------------------------------------------*)

(*
let rec membro l x =
  match l with
  [] -> false
  | h::t -> if x=h then true else membro t x;;

let rec conta x l =
  match l with
  [] -> 0
  | h::t -> (if x=h then 1 else 0) + conta x t;;

let rec append l1 l2 =
  match l1 with
  [] -> l2
  | h::t -> h :: append t l2;;

let rec soma_lista l =
  match l with
  [] -> 0
  | h::t -> h+soma_lista t;;

*)