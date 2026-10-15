open! Core

(* [never] marks "not seen in this direction"; times are ns since the epoch. *)
let never = Int.min_value

module Stable = struct
  module V1 = struct
    type t =
      { capacity : int
      ; mutable length : int
      ; keys : string array
      ; hashes : int array
      ; first : int array
      ; last_in : int array
      ; last_out : int array
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let create ~capacity =
  if capacity <= 0 then raise_s [%message "Recent.create: capacity must be positive"];
  { capacity
  ; length = 0
  ; keys = Array.create ~len:capacity ""
  ; hashes = Array.create ~len:capacity 0
  ; first = Array.create ~len:capacity never
  ; last_in = Array.create ~len:capacity never
  ; last_out = Array.create ~len:capacity never
  }
;;

let copy t =
  { t with
    keys = Array.copy t.keys
  ; hashes = Array.copy t.hashes
  ; first = Array.copy t.first
  ; last_in = Array.copy t.last_in
  ; last_out = Array.copy t.last_out
  }
;;

let capacity t = t.capacity
let length t = t.length

let find t key =
  let h = String.hash key in
  let rec go i =
    if i >= t.length
    then -1
    else if t.hashes.(i) = h && String.equal t.keys.(i) key
    then i
    else go (i + 1)
  in
  go 0
;;

let mem t key = find t key >= 0

let first_seen t key =
  let i = find t key in
  if i < 0 then None else Some (Time_ns.of_int_ns_since_epoch t.first.(i))
;;

let last_seen t i = Int.max t.last_in.(i) t.last_out.(i)

let evict_slot t =
  let best = ref 0 in
  for i = 1 to t.length - 1 do
    if last_seen t i < last_seen t !best then best := i
  done;
  !best
;;

let touch t key ~at ~inbound =
  let at = Time_ns.to_int_ns_since_epoch at in
  let i =
    match find t key with
    | i when i >= 0 -> i
    | _ ->
      let i =
        if t.length < t.capacity
        then (
          let i = t.length in
          t.length <- t.length + 1;
          i)
        else evict_slot t
      in
      t.keys.(i) <- key;
      t.hashes.(i) <- String.hash key;
      t.first.(i) <- at;
      t.last_in.(i) <- never;
      t.last_out.(i) <- never;
      i
  in
  if inbound
  then t.last_in.(i) <- Int.max t.last_in.(i) at
  else t.last_out.(i) <- Int.max t.last_out.(i) at
;;

let count_since t ~since ~inbound =
  let since = Time_ns.to_int_ns_since_epoch since in
  let last = if inbound then t.last_in else t.last_out in
  let n = ref 0 in
  for i = 0 to t.length - 1 do
    if last.(i) <> never && last.(i) >= since then incr n
  done;
  !n
;;

let keys t =
  List.init t.length ~f:Fn.id
  |> List.sort ~compare:(fun i j ->
    match Int.compare (last_seen t j) (last_seen t i) with
    | 0 -> Int.compare i j
    | c -> c)
  |> List.map ~f:(fun i -> t.keys.(i))
;;
