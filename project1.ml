(* Made by Jeffrey Do do000043 *)

type poly = PolyEmpty | PolyTerm of int * int * poly ;;

let isPolyOk p =
  let rec isPolyOking poly =
    match poly with
    | PolyEmpty -> true
    | PolyTerm(coef, expo, other) ->
      if (coef <> 0 && expo >= 0)
      then match other with
      | PolyEmpty -> true 
      | PolyTerm(otherCoef, otherExpo, _) ->
        if expo > otherExpo
        then isPolyOking other
        else false
      else false
in isPolyOking p;;

open List;;
exception MakePolyError;;
let makePoly i =
  let rec makePolying list = 
  match list with 
  | [] -> PolyEmpty
  | head :: tail -> 
    if tail = []
    then raise MakePolyError
    else let poly = PolyTerm(head, hd tail, makePolying (tl tail))
      in if isPolyOk poly
      then poly
      else raise MakePolyError
in makePolying i;; 

let polyAdd l r =
  let rec polyAdding (l, r) =
  match (l,r) with 
  | (PolyEmpty, PolyEmpty) -> PolyEmpty
  | (left, PolyEmpty) -> left
  | (PolyEmpty, right) -> right
  | (PolyTerm(lCoef, lExpo, lOther), PolyTerm(rCoef, rExpo, rOther)) ->
    if lExpo > rExpo
    then PolyTerm(lCoef, lExpo, polyAdding (lOther, PolyTerm(rCoef, rExpo, rOther)))
    else if rExpo > lExpo
      then PolyTerm(rCoef, rExpo, polyAdding (PolyTerm(lCoef, lExpo, lOther), rOther))
      else if lCoef + rCoef = 0
      then polyAdding (lOther, rOther)
      else PolyTerm(lCoef + rCoef, lExpo, polyAdding (lOther, rOther))
in polyAdding (l,r);;

let polyMinus r =
  let rec polyMinusing r = 
  match r with
  | PolyEmpty -> PolyEmpty
  | PolyTerm(coef, expo, other) ->
    PolyTerm(coef * -1, expo, polyMinusing other)
in polyMinusing r;; 

(* 
  isPoly Test Cases
  let p0 = PolyEmpty;;
  let p1 = PolyTerm(3, 5, PolyEmpty);;
  let p2 = PolyTerm(3,5, PolyTerm(2,6,PolyEmpty));;
  let p3 = PolyTerm(3,5, PolyTerm(2,4,PolyEmpty));;
  let p4 = PolyTerm(3,5, PolyTerm(2,-4,PolyEmpty));;
  let p5 = PolyTerm(3,-3, PolyTerm(2,-4,PolyEmpty));;
  let p6 = PolyTerm(3,6, PolyTerm(0,4,PolyTerm(7,2,PolyEmpty)));;
  let p7 = PolyTerm(3,6, PolyTerm(-1,4,PolyTerm(7,2,PolyEmpty)));;

  isPolyOk p0;; (* - : bool = true *)
  isPolyOk p1;; (* - : bool = true *)
  isPolyOk p2;; (* - : bool = false *)
  isPolyOk p3;; (* - : bool = true *)
  isPolyOk p4;; (* - : bool = false *)
  isPolyOk p5;; (* - : bool = false *)
  isPolyOk p6;; (* - : bool = false *)
  isPolyOk p7;; (* - : bool = true *)
*)

(* 
  makePoly Test Cases 
  let l0 = [];;
  let l1 = [2];;
  let l2 = [2; 1];;
  let l3 = [2; 1; 3];;
  let l4 = [2; 3; 3; 2];;
  let l5 = [2; 1; 3; 2];;

  makePoly l0;; (* - : poly = PolyEmpty *)
  (* makePoly l1;; (* Exception MakePolyError. *) *)
  makePoly l2;; (* - : poly = PolyTerm (2, 1, PolyEmpty) *)
  (* makePoly l3;; (* Exception MakePolyError. *) *)
  makePoly l4;; (* - : poly = PolyTerm (2, 3, PolyTerm (3, 2, PolyEmpty))*)
  (* makePoly l5;; (* Exception MakePolyError. *) *)
*)

(*
  polyAdd Test Cases
  let p0 = makePoly [1;2];;
  let p1 = makePoly [2;2];;
  let p2 = makePoly [];;
  let p3 = makePoly [3;3];;
  let p4 = makePoly [3;5;2;4;2;3;-1;2;5;0];;
  let p5 = makePoly [7;4;1;2;-4;1;-3;0];;

  polyAdd p0 p1;; (* - : poly = PolyTerm (3, 2, PolyEmpty) *)
  polyAdd p0 p2;; (* - : poly = PolyTerm (1, 2, PolyEmpty) *)
  polyAdd p2 p1;; (* - : poly = PolyTerm (2, 2, PolyEmpty) *)
  polyAdd p3 p1;; (* - : poly = PolyTerm (3, 3, PolyTerm (2, 2, PolyEmpty)) *)
  polyAdd p4 p5;; (* - : poly = PolyTerm (3, 5,
  PolyTerm (9, 4, PolyTerm (2, 3, PolyTerm (-4, 1, PolyTerm (2, 0, PolyEmpty))))) *)
*)

(*
  polyMinus TestCases
  let p0 = makePoly [7;4;1;2;-4;1;-3;0];;
  polyMinus p0; (* - : poly = 
  PolyTerm (-7, 4, PolyTerm (-1, 2, PolyTerm (4, 1, PolyTerm (3, 0, PolyEmpty)))) *)
*)