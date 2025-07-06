open Base
open Stdio

let num_blinks = 75

(* Get teh first half of a [len] digit integer [x] 
  
  Note that to get the first three digits '173' from '173452',
  we just need to do integer division by 10^3.
*)
let get_fst_half ~len x = x / Int.pow 10 (len / 2)

(* Get second half of a [len] digit integer [x] 
  
  This is simply the remainder if you divide by 10^(len//2).
  For instance, if x=173452, we get x / 10^3 = 173, so
  173452 mod 10^3 = 452.
*)
let get_snd_half ~len x = Int.rem x (Int.pow 10 (len / 2))

module Stone = struct
  type t =
    | Zero
    | Even_digits of int
    | Other of int
  [@@deriving sexp_of, compare, hash]

  let of_string s =
    if String.for_all s ~f:(fun c -> Char.( = ) c '0')
    then Zero
    else if String.length (Int.to_string @@ Int.of_string s) % 2 = 0
    then Even_digits (Int.of_string s)
    else Other (Int.of_string s)
  ;;

  let of_int x =
    if x = 0
    then Zero
    else if String.length (Int.to_string x) % 2 = 0
    then Even_digits x
    else Other x
  ;;

  let transform = function
    | Zero -> [ Other 1 ]
    | Even_digits x ->
      let len = String.length (Int.to_string x) in
      [ of_int (get_snd_half ~len x); of_int (get_fst_half ~len x) ]
    | Other x ->
      let value = 2024 * x in
      if String.length (Int.to_string value) % 2 = 0
      then [ Even_digits value ]
      else [ Other value ]
  ;;
end

let memoize (type a) (module M : Hashtbl.Key.S with type t = a) (f : a -> 'b) : a -> 'b =
  let table = Hashtbl.create (module M) in
  fun x -> Hashtbl.find_or_add table x ~default:(fun () -> f x)
;;

(* if Hashtbl.mem table x *)
(* then Hashtbl.find_exn table x *)
(* else ( *)
(*   let to_add = f x in *)
(*   Hashtbl.add_exn table ~key:x ~data:to_add; *)
(*   to_add) *)

let transform_memo = memoize (module Stone) Stone.transform

let read () =
  let s = In_channel.input_line_exn In_channel.stdin in
  let stones = String.split_on_chars ~on:[ ' ' ] s in
  List.map stones ~f:Stone.of_string
;;

let blink stones =
  let rec loop ~remaining stones =
    match remaining with
    | 0 -> stones
    | _ ->
      printf "It: %d\n" remaining;
      Out_channel.flush Out_channel.stdout;
      let new_stones =
        List.fold stones ~init:[] ~f:(fun acc stone ->
          match transform_memo stone with
          | [ x ] -> x :: acc
          | [ x; y ] -> x :: y :: acc
          | _ as lst -> lst @ acc)
      in
      loop ~remaining:(remaining - 1) new_stones
  in
  loop ~remaining:num_blinks stones
;;

module IntPair = struct
  type t = int * Stone.t [@@deriving sexp_of, compare, hash]
end

let blink_p2 stone =
  let h = Hashtbl.create (module IntPair) in
  let rec count (remaining, stone) =
    match Hashtbl.find h (remaining, stone) with
    | Some x -> x
    | None ->
      let res =
        match remaining with
        | 0 -> 1
        | _ ->
          (match Stone.transform stone with
           | [ x ] -> count (remaining - 1, x)
           | [ x; y ] -> count (remaining - 1, x) + count (remaining - 1, y)
           | _ -> assert false)
      in
      Hashtbl.set h ~key:(remaining, stone) ~data:res;
      res
  in
  let count_memo = memoize (module IntPair) count in
  count_memo (num_blinks, stone)
;;

let () =
  let num_stones =
    ignore blink;
    read ()
    |> (* part 1: blink |> List.length *) List.map ~f:blink_p2
    |> List.sum (module Int) ~f:Fn.id
  in
  printf "Stones: %d\n" num_stones
;;
