open! Core

module Stable = struct
  module V1 = struct
    type t =
      { p : float
      ; mutable count : int
      ; q : float array (** marker heights; the raw values while [count < 5] *)
      ; n : float array (** marker positions, 1-based *)
      ; desired : float array
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let create ~p =
  if not Float.(p > 0. && p < 1.)
  then raise_s [%message "P2_quantile: p outside (0, 1)" (p : float)];
  { p
  ; count = 0
  ; q = Array.create ~len:5 0.
  ; n = [| 1.; 2.; 3.; 4.; 5. |]
  ; desired = [| 1.; 1. +. (2. *. p); 1. +. (4. *. p); 3. +. (2. *. p); 5. |]
  }
;;

let copy t =
  { t with q = Array.copy t.q; n = Array.copy t.n; desired = Array.copy t.desired }
;;

let p t = t.p
let count t = t.count

let parabolic t i d =
  let q = t.q
  and n = t.n in
  q.(i)
  +. (d
      /. (n.(i + 1) -. n.(i - 1))
      *. (((n.(i) -. n.(i - 1) +. d) *. (q.(i + 1) -. q.(i)) /. (n.(i + 1) -. n.(i)))
          +. ((n.(i + 1) -. n.(i) -. d) *. (q.(i) -. q.(i - 1)) /. (n.(i) -. n.(i - 1))))
     )
;;

let linear t i d =
  let j = i + Float.to_int d in
  t.q.(i) +. (d *. (t.q.(j) -. t.q.(i)) /. (t.n.(j) -. t.n.(i)))
;;

(* How far each marker's desired position moves per observation. *)
let increment p = function
  | 0 -> 0.
  | 1 -> p /. 2.
  | 2 -> p
  | 3 -> (1. +. p) /. 2.
  | _ -> 1.
;;

let add t x =
  if t.count < 5
  then (
    t.q.(t.count) <- x;
    t.count <- t.count + 1;
    if t.count = 5 then Array.sort t.q ~compare:Float.compare)
  else (
    t.count <- t.count + 1;
    let q = t.q
    and n = t.n in
    let k =
      if Float.(x < q.(0))
      then (
        q.(0) <- x;
        0)
      else if Float.(x >= q.(4))
      then (
        q.(4) <- x;
        3)
      else (
        let k = ref 0 in
        while Float.(x >= q.(Int.succ !k)) do
          incr k
        done;
        !k)
    in
    for i = k + 1 to 4 do
      n.(i) <- n.(i) +. 1.
    done;
    for i = 0 to 4 do
      t.desired.(i) <- t.desired.(i) +. increment t.p i
    done;
    for i = 1 to 3 do
      let d = t.desired.(i) -. n.(i) in
      let up = n.(i + 1) -. n.(i)
      and down = n.(i - 1) -. n.(i) in
      if Float.((d >= 1. && up > 1.) || (d <= -1. && down < -1.))
      then (
        let d = Float.(if d > 0. then 1. else -1.) in
        let candidate = parabolic t i d in
        let below = q.(i - 1)
        and above = q.(i + 1) in
        q.(i)
        <- (if Float.(below < candidate && candidate < above)
            then candidate
            else linear t i d);
        n.(i) <- n.(i) +. d)
    done)
;;

let estimate t =
  if t.count = 0
  then None
  else if t.count >= 5
  then Some t.q.(2)
  else (
    let seen = Array.sub t.q ~pos:0 ~len:t.count in
    Array.sort seen ~compare:Float.compare;
    (* nearest rank: the smallest value with at least p of the values at or below it *)
    let rank = Float.iround_up_exn (t.p *. Float.of_int t.count) in
    Some seen.(Int.max 0 (rank - 1)))
;;
