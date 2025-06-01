open Base
open Stdio

module MemoryLocation = struct
  type t =
    | File of int
    | Empty of int
  [@@deriving sexp]

  let get = function
    | File x | Empty x -> x
  ;;
end

open MemoryLocation

let read () =
  let input = Stdio.In_channel.input_all Stdio.stdin |> String.to_list in
  let rec parse acc is_file = function
    | hd :: tl ->
      let value = Char.to_string hd |> Int.of_string_opt in
      (match value with
       | Some x ->
         let next_value = if is_file then File x else Empty x in
         parse (next_value :: acc) (not is_file) tl
       | None -> acc)
    | [] -> acc
  in
  print_s [%sexp (input : char list)];
  let r = parse [] true input |> List.rev |> Array.of_list in
  print_s [%sexp (r : t array)];
  printf "Length %d\n" (Array.length r);
  r
;;

let expand memory =
  let length = Array.fold memory ~init:0 ~f:(fun acc x -> acc + get x) in
  let expanded = Array.create ~len:length "." in
  printf "Expanded has length %d\n" (Array.length expanded);
  let i = ref 0 in
  let id = ref 0 in
  Array.iter memory ~f:(fun memory_loc ->
    match memory_loc with
    | File x ->
      for offset = 0 to x - 1 do
        expanded.(!i + offset) <- Int.to_string !id
      done;
      printf "Set values %d - %d " !i (!i + x);
      id := !id + 1;
      i := !i + x;
      printf "Updated id=%d i=%d\n" !id !i
    | Empty x ->
      for offset = 0 to x - 1 do
        expanded.(!i + offset) <- "."
      done;
      printf "Set values %d - %d" !i (!i + x);
      i := !i + x;
      printf "Updated id=%d i=%d\n" !id !i);
  print_s [%sexp (expanded : string array)];
  expanded
;;

let arrange memory =
  let to_ = ref 0 in
  let from_ = ref (Array.length memory - 1) in
  while !to_ < !from_ do
    if String.(memory.(!to_) <> ".")
    then Int.incr to_
    else if String.(memory.(!from_) = ".")
    then Int.decr from_
    else Array.swap memory !to_ !from_
  done;
  print_s [%sexp (memory : string array)];
  memory
;;

let cut memory =
  let cut_idx = ref 0 in
  while !cut_idx < Array.length memory && not String.(memory.(!cut_idx) = ".") do
    cut_idx := !cut_idx + 1
  done;
  Array.sub memory ~pos:0 ~len:!cut_idx |> Array.map ~f:Int.of_string
;;

let checksum memory =
  Array.foldi memory ~init:0 ~f:(fun idx acc value -> acc + (idx * value))
;;

let () =
  let result = read () |> expand |> arrange |> cut |> checksum in
  printf "Result: %d\n" result
;;
