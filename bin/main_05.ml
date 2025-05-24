open Base
open Stdio

let read () =
  let parts =
    In_channel.input_all In_channel.stdin
    |> String.substr_replace_first ~pattern:"\n\n" ~with_:"X"
    |> String.split ~on:'X'
  in
  let dependencies, updates =
    match parts with
    | [ deps; updates ] ->
      let deps_list = String.split_lines deps |> List.map ~f:(String.split ~on:'|') in
      let deps_tuple =
        List.map deps_list ~f:(fun dep ->
          match dep with
          | [ fst; snd ] -> fst, snd
          | _ :: _ | [] -> failwith "could not parse deps and updates")
      in
      let updates_lst =
        String.split_lines updates |> List.map ~f:(String.split ~on:',')
      in
      deps_tuple, updates_lst
    | _ :: _ | [] -> failwith "could not parse deps and updates"
  in
  dependencies, updates
;;

let dfs_match ~graph page others : bool =
  let rec dfs visited current : string list =
    let neighbors = Hashtbl.find_multi graph current in
    List.fold neighbors ~init:visited ~f:(fun acc neighbor ->
      if not (List.mem acc neighbor ~equal:String.equal)
      then dfs (neighbor :: acc) neighbor
      else acc)
  in
  let rec aux found page others =
    match others with
    | fst :: rst ->
      let deps_of_page = dfs [] page in
      if List.mem deps_of_page fst ~equal:String.equal then false else aux found page rst
    | [] -> found
  in
  aux true page others
;;

(* Part 1*)
let string_in_list lst value = List.mem lst value ~equal:String.equal

let build_inverse_deps_for_pages ~deps pages =
  let graph = Hashtbl.create (module String) in
  List.filter deps ~f:(fun (from, to_) ->
    string_in_list pages from && string_in_list pages to_)
  |> List.iter ~f:(fun (from, to_) -> Hashtbl.add_multi graph ~key:to_ ~data:from);
  graph
;;

let no_inverse_dependencies ~deps pages =
  (* Create filtered adjacency graph.
     Consider only dependencies that relate to the update in question *)
  let graph = build_inverse_deps_for_pages ~deps pages in
  let rec aux = function
    | page :: others -> dfs_match ~graph page others && aux others
    | [] -> true
  in
  aux pages
;;

let find_valid_updates (deps, updates) =
  List.filter updates ~f:(no_inverse_dependencies ~deps)
  |> List.map ~f:(List.map ~f:Int.of_string)
;;

let get_medians updates =
  List.fold updates ~init:[] ~f:(fun acc pages ->
    let median = List.nth pages (List.length pages / 2) |> Option.value_exn in
    median :: acc)
;;

let sum lst = List.fold lst ~init:0 ~f:(fun acc x -> x + acc)

(* Part 2 *)
let depends_on ~graph page other = dfs_match ~graph page [ other ]

let fix_update ~deps update =
  let graph = build_inverse_deps_for_pages ~deps update in
  ignore deps;
  let update_arr = Array.of_list update in
  let length = Array.length update_arr - 1 in
  for start_index = length downto 0 do
    let curr_index = ref start_index in
    for remain_index = start_index + 1 to length do
      if depends_on ~graph update_arr.(remain_index) update_arr.(!curr_index)
      then (
        Array.swap update_arr !curr_index remain_index;
        curr_index := remain_index)
    done
  done;
  update_arr |> List.of_array
;;

let correct_invalid_updates (deps, updates) =
  List.filter updates ~f:(fun update -> not (no_inverse_dependencies ~deps update))
  |> List.map ~f:(fix_update ~deps)
  |> List.map ~f:(List.map ~f:Int.of_string)
;;

let () =
  ignore find_valid_updates;
  (* Part 1 *)
  (* let result_part1 = read () |> find_valid_updates |> get_medians |> sum in *)
  (* printf "Result: %d\n" result_part1; *)
  (* Part 2*)
  let result_part2 = read () |> correct_invalid_updates |> get_medians |> sum in
  printf "Result: %d\n" result_part2
;;
