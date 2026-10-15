open! Core

module Stable = struct
  module V1 = struct
    type t =
      { width : int (** nanoseconds *)
      ; buckets : int
      ; fields : int
      ; mutable head : int
      (** absolute index (time / width) of the newest bucket; [-1] when empty *)
      ; data : int array (** [buckets * fields], bucket-major *)
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let create ~width ~buckets ~fields =
  let width = Time_ns.Span.to_int_ns width in
  if width <= 0 || buckets <= 0 || fields <= 0
  then
    raise_s [%message "Window.create: positive sizes required" (width : int) (buckets : int) (fields : int)];
  { width; buckets; fields; head = -1; data = Array.create ~len:(buckets * fields) 0 }
;;

let copy t = { t with data = Array.copy t.data }
let width t = Time_ns.Span.of_int_ns t.width
let buckets t = t.buckets
let fields t = t.fields
let bucket_of t time = Time_ns.to_int_ns_since_epoch time / t.width

let clear_bucket t b =
  let base = b mod t.buckets * t.fields in
  Array.fill t.data ~pos:base ~len:t.fields 0
;;

(* Moves the newest bucket to [b], clearing every bucket that falls out or is skipped. *)
let advance t b =
  if b > t.head
  then (
    if t.head < 0 || b - t.head >= t.buckets
    then Array.fill t.data ~pos:0 ~len:(Array.length t.data) 0
    else
      for k = t.head + 1 to b do
        clear_bucket t k
      done;
    t.head <- b)
;;

(* The data offset of [time]'s bucket, or -1 when it is older than the window. *)
let slot t time =
  let b = bucket_of t time in
  advance t b;
  if b <= t.head - t.buckets then -1 else b mod t.buckets * t.fields
;;

let add t ~at ~field v =
  if v <> 0
  then (
    let base = slot t at in
    if base >= 0 then t.data.(base + field) <- t.data.(base + field) + v)
;;

let add_many t ~at values =
  let base = slot t at in
  if base >= 0
  then
    for i = 0 to Int.min (Array.length values) t.fields - 1 do
      t.data.(base + i) <- t.data.(base + i) + values.(i)
    done
;;

let sum t ~now ~field ~last =
  if t.head < 0
  then 0
  else (
    let nb = bucket_of t now in
    let last = Int.min last t.buckets in
    let lo = Int.max (nb - last + 1) (t.head - t.buckets + 1) in
    let hi = Int.min nb t.head in
    let acc = ref 0 in
    for b = lo to hi do
      acc := !acc + t.data.((b mod t.buckets * t.fields) + field)
    done;
    !acc)
;;

let total t ~now ~field = sum t ~now ~field ~last:t.buckets
