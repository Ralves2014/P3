type 'a abp = F | N of ('a * 'a abp * 'a abp);;

let rec lookup = function F-> (function _ -> false) 
  | N (x, fe, fd) -> function e -> if e=x then true else if x<e then lookup fe e else lookup fd e

(* let rec insert e = function F -> N (e, F, F)
  | N (x, fe, fd) -> if e<x then N(x, (insert e fe), fd) else N(x, fe, (insert e fd)) *)

let rec insert = function F -> (function e -> N (e, F, F))
  | N (x, fe, fd) -> function e -> if e<x then N(x, (insert fe e), fd) else N(x, fe, (insert fd e))

let rec minValue = function 
  | F -> raise Not_Found
  | (x, fe, )

let rec delete = function 
  | F -> (function _ -> F)
  | N (x, fe, fd) -> function e -> 
    if e < x then N (x, delete fe e, fd)
    else if e > x then N (x, fe, delete fd e)
    else 
      match (fe, fd) with
      | (F,F) -> F 
      | (fe, F) -> fe 
      | (F, fd) -> fd
      | (_,_) -> let newValue = minValue right in N (newValue, fe, delete fe newValue)
                                    