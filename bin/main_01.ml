open Stdio
open Base

(* Part 1 *)
let int_of_string_safe x =
  match Int.of_string_opt x with
  | Some value -> value
  | None -> failwith ("not a valid int: " ^ x)
;;

let build_lists () =
  let rec aux list1 list2 =
    match In_channel.input_line In_channel.stdin with
    | None | Some "" -> list1, list2
    | Some value ->
      (match String.split ~on:' ' value with
       | item1 :: "" :: "" :: item2 :: _ ->
         let int1 = int_of_string_safe item1 in
         let int2 = int_of_string_safe item2 in
         aux (int1 :: list1) (int2 :: list2)
       | _ -> failwith "unexpected split values")
  in
  let list1, list2 = aux [] [] in
  List.rev list1, List.rev list2
;;

let get_total_distance list1 list2 =
  let sorted_list1 = List.sort list1 ~compare:Int.compare in
  let sorted_list2 = List.sort list2 ~compare:Int.compare in
  let distance =
    List.fold2 sorted_list1 sorted_list2 ~init:0 ~f:(fun acc a b -> acc + Int.abs (a - b))
  in
  match distance with
  | List.Or_unequal_lengths.Ok value -> value
  | List.Or_unequal_lengths.Unequal_lengths -> failwith "unequal lengths"
;;

(* Part 2*)
let similarity_score list1 list2 =
  let list2_lookup =
    Hashtbl.group
      (module Int)
      ~get_key:(fun value -> value)
      ~get_data:(fun _ -> 1)
      ~combine:(fun a b -> a + b)
      list2
  in
  List.fold list1 ~init:0 ~f:(fun acc x ->
    match Hashtbl.find list2_lookup x with
    | Some value -> acc + (x * value)
    | None -> acc)
;;

let () =
  let list1, list2 = build_lists () in
  let distance = get_total_distance list1 list2 in
  let score = similarity_score list1 list2 in
  printf "distance: %d\n" distance;
  printf "score: %d\n" score
;;
