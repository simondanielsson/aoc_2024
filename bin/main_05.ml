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
      then (
        printf "Visiting neighbor %s from current %s\n" neighbor current;
        dfs (neighbor :: acc) neighbor)
      else acc)
  in
  let rec aux found page others =
    match others with
    | fst :: rst ->
      printf "\n-- Running dfs on page %s --\n\n" page;
      let deps_of_page = dfs [] page in
      printf "Dependencies of page %s:\n" page;
      print_s [%sexp (deps_of_page : string list)];
      if List.mem deps_of_page fst ~equal:String.equal then false else aux found page rst
    | [] -> found
  in
  aux true page others
;;

let string_in_list lst value = List.mem lst value ~equal:String.equal

let no_inverse_dependencies ~deps pages =
  (* Create filtered adjacency graph.
     Consider only dependencies that relate to the update in question *)
  let graph = Hashtbl.create (module String) in
  List.filter deps ~f:(fun (from, to_) ->
    string_in_list pages from && string_in_list pages to_)
  |> List.iter ~f:(fun (from, to_) -> Hashtbl.add_multi graph ~key:to_ ~data:from);
  let rec aux = function
    | page :: others -> dfs_match ~graph page others && aux others
    | [] -> true
  in
  printf "Checking deps of pages:\n";
  print_s [%sexp (pages : string list)];
  let had_no_dependencies = aux pages in
  printf "had_no_deps=%s\n" (Bool.to_string had_no_dependencies);
  had_no_dependencies
;;

let find_valid_updates (deps, updates) =
  print_s [%sexp (deps : (string * string) list)];
  print_s [%sexp (updates : string list list)];
  (* Filter updates that are valid *)
  List.filter updates ~f:(no_inverse_dependencies ~deps)
  |> List.map ~f:(List.map ~f:Int.of_string)
;;

(*
   For each update
1. Filter out the deps that include two page from update
2. Build (inverse) dependency graph (adjacency matrix through hashtbl)
3. For each page
      For each subsequent page
          if page depends on subsequent page 
            return acc
   pages :: acc


helper page_depends_on_other graph page others =
  // perform bfs
  let dfs visited found page = 
    for each neighbor of page
      if neighbor not visited 
        dfs (neighbor::visited) found neighbor
      else 
        visited, found
  let dependencies_of_page, found = dfs [] false page in
  found
*)

let get_medians updates =
  printf "Calculating medians for %d updates\n" (List.length updates);
  List.fold updates ~init:[] ~f:(fun acc pages ->
    let median = List.nth pages (List.length pages / 2) |> Option.value_exn in
    median :: acc)
;;

let sum lst =
  printf "Summing medians of %d lists\n" (List.length lst);
  List.fold lst ~init:0 ~f:(fun acc x -> x + acc)
;;

let () =
  let result = read () |> find_valid_updates |> get_medians |> sum in
  printf "Result: %d" result
;;
