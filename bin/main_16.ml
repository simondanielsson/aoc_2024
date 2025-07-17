open Core
open Stdio

module Constants = struct
  let turn_weight = 1000
  let straight_weight = 1
end

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, hash, compare, equal]

  let all = [ Up; Down; Left; Right ]

  let turns = function
    | Up | Down -> [ Left; Right ]
    | Left | Right -> [ Up; Down ]
  ;;
end

module IntPair = struct
  type t = int * int [@@deriving sexp_of, hash, compare, equal]
end

module Pos = struct
  type t =
    { pos : IntPair.t
    ; dir : Dir.t
    }
  [@@deriving sexp_of, compare, equal, hash]

  let same_pos t t' = fst t.pos = fst t'.pos && snd t.pos = snd t'.pos

  let next t =
    let r = fst t.pos in
    let c = snd t.pos in
    match t.dir with
    | Up -> r - 1, c
    | Down -> r + 1, c
    | Left -> r, c - 1
    | Right -> r, c + 1
  ;;
end

module PosWithDist = struct
  type t =
    { pos : Pos.t
    ; dist : int
    ; parent : Pos.t
    }
  [@@deriving sexp_of]

  let with_total_dist (cur : t) (neighbor : Pos.t) =
    if Dir.equal cur.pos.dir neighbor.dir
    then { pos = neighbor; dist = cur.dist + Constants.straight_weight; parent = cur.pos }
    else { pos = neighbor; dist = cur.dist + Constants.turn_weight; parent = cur.pos }
  ;;

  let compare_dist t1 t2 = Int.compare t1.dist t2.dist
end

module PosType = struct
  type t =
    | Wall
    | Start
    | End
    | Empty
  [@@deriving sexp_of, hash, compare, equal]

  let of_char = function
    | '#' -> Wall
    | 'S' -> Start
    | 'E' -> End
    | '.' -> Empty
    | _ -> failwith "invalid character"
  ;;

  let nonwall = function
    | Empty | Start | End -> true
    | Wall -> false
  ;;
end

type graph = Pos.t list Hashtbl.M(Pos).t [@@deriving sexp_of]

let read () =
  let lines = In_channel.(input_lines stdin) in
  let rows = List.length lines in
  let cols = String.length (List.hd_exn lines) in
  let matrix =
    Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> PosType.Empty))
  in
  List.iteri lines ~f:(fun r row ->
    String.iteri row ~f:(fun c chr -> matrix.(r).(c) <- PosType.of_char chr));
  matrix
;;

let create_graph (matrix : PosType.t array array) =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let g : graph = Hashtbl.create (module Pos) in
  let within_bounds r c = r >= 0 && r < rows - 1 && c > 0 && c < cols - 1 in
  let start_pos = ref None in
  let end_pos = ref [] in
  let add_adjacents r c =
    List.iter Dir.all ~f:(fun dir ->
      let this = Pos.{ pos = r, c; dir } in
      (* Add turns *)
      List.iter (Dir.turns dir) ~f:(fun turn ->
        Hashtbl.add_multi g ~key:this ~data:Pos.{ pos = r, c; dir = turn });
      (* Add move forward*)
      let r', c' = Pos.next this in
      if within_bounds r' c' && PosType.nonwall matrix.(r').(c')
      then Hashtbl.add_multi g ~key:this ~data:Pos.{ pos = r', c'; dir })
  in
  Array.iteri matrix ~f:(fun r row ->
    Array.iteri row ~f:(fun c pos ->
      match pos with
      | Start ->
        (*Always start facing east *)
        start_pos := Some Pos.{ pos = r, c; dir = Right };
        add_adjacents r c
      | End ->
        (* Allow ending in any direction*)
        end_pos
        := List.fold Dir.all ~init:[] ~f:(fun acc dir -> Pos.{ pos = r, c; dir } :: acc)
      | Empty -> add_adjacents r c
      | _ -> ()));
  g, Option.value_exn !start_pos, !end_pos
;;

(* Part 1 *)
let shortest_path (g, start, ends) =
  let visited = ref [] in
  let heap = Pairing_heap.create ~cmp:PosWithDist.compare_dist () in
  let rec dijkstras (cur : PosWithDist.t) =
    if List.mem !visited cur.pos ~equal:Pos.equal
    then (
      let next = Pairing_heap.pop heap |> Option.value_exn in
      dijkstras next)
    else if List.exists ends ~f:(fun _end -> Pos.same_pos cur.pos _end)
    then cur.dist
    else (
      visited := cur.pos :: !visited;
      let neighbors = Hashtbl.find_exn g cur.pos in
      List.iter neighbors ~f:(fun neighbor ->
        Pairing_heap.add heap (PosWithDist.with_total_dist cur neighbor));
      let next = Pairing_heap.pop heap |> Option.value_exn in
      dijkstras next)
  in
  dijkstras PosWithDist.{ pos = start; dist = 0; parent = start }
;;

(* Part 2
  
  1. We need to track the parent(s) of each node so we can trace back and count the nodes.
    A node can have multiple parents if the cost to reach that node is the same for two paths.
    This can happen if the [next] is the same as the current best at the same distance, 
    modulo 1-2 turns to get into the same direction.
    If we see that this is the case, i.e. next.pos=cur_best.pos and next.dist+turns = cur_best.dist
      then we append cur as the parent of next
    Note that we discard any direction information: we only care about positions.
  2. We need to find *all* shortest paths.
    Let's for simplicity assume the End node only has a single parent node
*)
let shortest_path_part2 (g, start, ends) =
  let visited = ref [] in
  let heap = Pairing_heap.create ~cmp:PosWithDist.compare_dist () in
  let parents = Hashtbl.create (module Pos) in
  let best = Hashtbl.create (module IntPair) in
  let rec dijkstras (cur : PosWithDist.t) =
    if fst cur.pos.pos = 6 && snd cur.pos.pos = 15 then print_endline "6 - 15";
    if fst cur.pos.pos = 7 && snd cur.pos.pos = 15 then print_endline "7 - 15";
    if List.mem !visited cur.pos ~equal:Pos.equal
    then (
      let next = Pairing_heap.pop heap |> Option.value_exn in
      printf
        "Already visited. cur=%s. next: %s. cdtc=%d\n"
        (PosWithDist.sexp_of_t cur |> Sexp.to_string)
        (PosWithDist.sexp_of_t next |> Sexp.to_string)
        (Hashtbl.find_exn best cur.pos.pos);
      (* Add as parent if this is another way to get to current node, modulo one turn *)
      let best_distance_to_cur = Hashtbl.find_exn best cur.pos.pos in
      if cur.dist <= best_distance_to_cur + Constants.turn_weight
      then (
        printf "adding ctdc";
        Hashtbl.add_multi parents ~key:cur.pos ~data:cur.parent);
      dijkstras next)
    else if List.exists ends ~f:(fun _end -> Pos.same_pos cur.pos _end)
    then cur.dist
    else (
      visited := cur.pos :: !visited;
      if not (Hashtbl.mem best cur.pos.pos)
      then Hashtbl.add_exn best ~key:cur.pos.pos ~data:cur.dist;
      let neighbors = Hashtbl.find_exn g cur.pos in
      List.iter neighbors ~f:(fun neighbor ->
        Pairing_heap.add heap (PosWithDist.with_total_dist cur neighbor));
      let next = Pairing_heap.pop heap |> Option.value_exn in
      let parents_of_next = Hashtbl.find_multi parents next.pos in
      (* TODO: we need to add parents if they have the same dist modulo 1-2 turns! *)
      (* If the parent is already in the list of parents to the node, don't do anythin*)
      if not
           (* Don't add duplicates (ignore direction)*)
           (List.mem parents_of_next next.parent ~equal:Pos.equal
            || List.mem !visited next.pos ~equal:Pos.equal)
      then (
        printf "Assigning  ";
        print_s [%sexp (next.parent : Pos.t)];
        printf " as parent to ";
        print_s [%sexp (next.pos : Pos.t)];
        Hashtbl.add_multi parents ~key:next.pos ~data:next.parent;
        let new_parents = Hashtbl.find_multi parents next.pos in
        print_s [%sexp (new_parents : Pos.t list)])
      else printf "Nothing \n";
      dijkstras next)
  in
  let s = PosWithDist.{ pos = start; dist = 0; parent = start } in
  Hashtbl.add_multi parents ~key:start ~data:start;
  let _ = dijkstras s in
  printf "Parents:\n";
  print_s [%sexp (parents : (Pos.t, Pos.t list) Hashtbl.t)];
  let merged_parents = Hashtbl.create (module IntPair) in
  let merge_parents () =
    Hashtbl.iteri parents ~f:(fun ~key ~data ->
      List.iter data ~f:(fun parent ->
        Hashtbl.add_multi merged_parents ~key:key.pos ~data:parent.pos))
  in
  print_endline "Merged parents:";
  merge_parents ();
  print_s [%sexp (merged_parents : (IntPair.t, IntPair.t list) Hashtbl.t)];
  let traverse_visited : IntPair.t list ref = ref [] in
  let rec traverse_path (pos : Pos.t) count =
    let cur_parents = Hashtbl.find_multi parents pos in
    List.fold cur_parents ~init:count ~f:(fun acc parent ->
      printf "Trying parent (%d, %d)\n" (fst parent.pos) (snd parent.pos);
      if not (List.mem !traverse_visited parent.pos ~equal:IntPair.equal)
      then (
        printf "(%d) " acc;
        print_s [%sexp (parent : Pos.t)];
        traverse_visited := parent.pos :: !traverse_visited;
        traverse_path parent (acc + 1))
      else traverse_path parent acc)
  in
  let relevant_ends =
    List.map ends ~f:(fun _end -> if Hashtbl.mem parents _end then Some _end else None)
  in
  let _end = List.hd_exn @@ List.filter_opt relevant_ends in
  print_s [%sexp (_end : Pos.t)];
  traverse_path _end 1
;;

let () =
  (* Part 1 *)
  ignore shortest_path;
  (* let result = read () |> create_graph |> shortest_path in *)
  (* Part 2 *)
  let result = read () |> create_graph |> shortest_path_part2 in
  printf "Result: %d\n" result
;;
