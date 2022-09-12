(*
Christopher Kim
114607126
chriskim
CSE 216 HW 1
*)

let rec pow (x: int) n = 
  if n = 0 then 1 else x * pow x (n-1);;

let rec float_pow (x: float) n = 
  if n = 0.0 then 1.0 else x *. float_pow x (n-.1.0);;


(**slightly different, might want to check*)
let rec compress (list: 'a list) : ('a list) =  
  match list with
  |first::(second::t) -> if first = second then compress (second::t) else first::(compress (second::t))
  |_ -> list;;


let rec remove_if (list: 'a list) (pred: ('a -> bool)) : ('a list) = 
  match list with
  |[] -> []
  |h::t -> if (pred h) = true then (remove_if t pred) else h::(remove_if t pred);;


let rec slice (list: 'a list) (i: int) (j: int) = 
  (**if (j < i) then [] else if (i<0) then list else*)
  match list with
  |[] -> []
  |h::t -> if (i <= 0 && j > 0) then h::(slice t (i-1) (j-1)) else (slice t (i-1) (j-1));;
  



let rec equivs (pred: 'a -> 'a -> bool) (list: 'a list) = 
  let rec eHelper (pred: 'a -> bool) (list: 'a list) = 
    match list with
    |[] -> []
    |h::t -> if (pred h) then h::(eHelper pred t) else (eHelper pred t)
  in 
  let rec remove_intersection (list_one: 'a list) (list_two: 'a list) = 
    match list_one with
    |[] -> list_two
    |h::t -> let no_h_list_two = (remove_if list_two ((=)h)) in (remove_intersection t no_h_list_two)
  in 
  match list with
  |[] -> [[]]
  |h::t -> let list_one = (eHelper (pred h) list) 
      in 
      let list_two = (remove_intersection list_one t) in
      match list_two with
      |[] -> [list_one]
      |h::t -> list_one :: (equivs pred list_two);; 

let goldbachpair sum =
  let is_prime (num: int) =
    let rec prime_helper init_num decrement =
      match decrement with
      |1 -> true    
      |_ ->  let modular = (init_num mod decrement) in if(modular != 0 && prime_helper init_num (decrement-1)) then true else false
    in
    match num with
    |0 -> false
    |1 -> false
    |_ -> prime_helper num (num-1)
  in
  let rec helper num_one =
    if (is_prime num_one && is_prime (sum - num_one)) then (num_one, sum - num_one) else helper (num_one + 1)
  in
  helper 2;;


let rec equiv_on f g lst =
  match lst with
  |[] -> true
  |h::t -> if ((f h) = (g h)) then (equiv_on f g t) else false;; 

let rec pairwisefilter cmp lst = 
  match lst with
  |[] -> [] 
  |first::(second::t) -> (cmp first second)::(pairwisefilter cmp t)
  |h::_ -> [h];;


let rec polynomial list =
  fun poly -> match list with
    |[] -> 0
    |h :: t -> ((fst h) * (pow poly (snd h))) + ((polynomial t) poly);;




let rec powerset list =
  let rec helper func list =
    match list with
    |[] -> []
    |h::t -> (func h) :: (helper func t)
  in
  let rec concat list_one list_two =
    match list_one with
    |[] -> list_two
    |h::t -> h::(concat list_two t)
  in
  match list with
  | [] -> [[]]
  | h::t -> concat (powerset t) (helper (fun o -> h::o) (powerset t));;

  