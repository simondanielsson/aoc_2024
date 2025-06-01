open Base
open Stdio

module MemoryLocation = struct
  type t =
    | File of
        { len : int
        ; id : int
        }
    | Empty of int
  [@@deriving sexp]

  let get_len = function
    | File x -> x.len
    | Empty x -> x
  ;;
end

open MemoryLocation

let read () =
  let input = Stdio.In_channel.input_all Stdio.stdin |> String.to_list in
  let rec parse acc is_file id = function
    | hd :: tl ->
      let value = Char.to_string hd |> Int.of_string_opt in
      (match value with
       | Some x ->
         let next_value = if is_file then File { len = x; id } else Empty x in
         let next_id = if is_file then id else id + 1 in
         parse (next_value :: acc) (not is_file) next_id tl
       | None -> acc)
    | [] -> acc
  in
  print_s [%sexp (input : char list)];
  let r = parse [] true 0 input |> Array.of_list_rev in
  print_s [%sexp (r : t array)];
  printf "Length %d\n" (Array.length r);
  r
;;

let rearrange (memory : t array) =
  let copy = ref memory in
  let upto = Array.length memory in
  Out_channel.flush Out_channel.stdout;
  for n = 0 to upto do
    printf "n=%d\n" n;
    Out_channel.flush Out_channel.stdout;
    let tmp = ref [] in
    let to_ = ref 0 in
    let from_ = ref (Array.length memory - 1) in
    let swapped = ref false in
    while !to_ < !from_ && not !swapped do
      print_s [%sexp (List.rev !tmp : t list)];
      match !copy.(!to_) with
      | File _ as f ->
        tmp := f :: !tmp;
        printf "%d (%d, %d) Appending file\n" n !to_ !from_;
        Int.incr to_
      | Empty width ->
        (match !copy.(!from_) with
         | Empty _ ->
           printf "%d (%d, %d) Empty empty, decr from\n" n !to_ !from_;
           Int.decr from_
         | File { len; _ } as f ->
           if len < width
           then (
             printf "%d (%d, %d) swapping\n" n !to_ !from_;
             tmp := Empty (width - len) :: f :: !tmp;
             swapped := true)
           else printf "%d (%d, %d) could not swap\n" n !to_ !from_;
           Int.decr from_)
    done;
    print_s [%sexp (!tmp : t list)];
    copy := Array.of_list_rev !tmp
  done;
  !copy
;;

let expand_and_arrange memory =
  let length = Array.fold memory ~init:0 ~f:(fun acc x -> acc + get_len x) in
  let expanded = Array.create ~len:length "." in
  let rearranged_memory = rearrange memory in
  printf "Expanded has length %d\n" (Array.length expanded);
  let i = ref 0 in
  Array.iter rearranged_memory ~f:(fun memory_loc ->
    match memory_loc with
    | File { len; id } ->
      for offset = 0 to len - 1 do
        expanded.(!i + offset) <- Int.to_string id
      done;
      printf "Set values %d - %d " !i (!i + len);
      i := !i + len;
      printf "Updated id=%d i=%d\n" id !i
    | Empty x ->
      for offset = 0 to x - 1 do
        expanded.(!i + offset) <- "."
      done;
      printf "Set values %d - %d" !i (!i + x);
      i := !i + x);
  print_s [%sexp (expanded : string array)];
  expanded
;;

let cut memory =
  let cut_idx = ref (Array.length memory - 1) in
  while !cut_idx > 0 && String.(memory.(!cut_idx) = ".") do
    Int.decr cut_idx
  done;
  Array.sub memory ~pos:0 ~len:!cut_idx |> Array.map ~f:Int.of_string
;;

let checksum memory =
  Array.foldi memory ~init:0 ~f:(fun idx acc value -> acc + (idx * value))
;;

let () =
  let result = read () |> expand_and_arrange |> cut |> checksum in
  printf "Result: %d\n" result
;;
