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
    ; parent : IntPair.t
    }
  [@@deriving sexp_of]

  let with_total_dist (cur : t) (neighbor : Pos.t) =
    if Dir.equal cur.pos.dir neighbor.dir
    then
      { pos = neighbor
      ; dist = cur.dist + Constants.straight_weight
      ; parent = cur.pos.pos
      }
    else { pos = neighbor; dist = cur.dist + Constants.turn_weight; parent = cur.pos.pos }
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
  dijkstras PosWithDist.{ pos = start; dist = 0; parent = start.pos }
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
  let parents = Hashtbl.create (module IntPair) in
  let rec dijkstras (cur : PosWithDist.t) (cur_best : PosWithDist.t) =
    if List.mem !visited cur.pos ~equal:Pos.equal
    then (
      let next = Pairing_heap.pop heap |> Option.value_exn in
      print_endline "Already visited";
      dijkstras next cur_best)
    else if List.exists ends ~f:(fun _end -> Pos.same_pos cur.pos _end)
    then cur.dist
    else (
      visited := cur.pos :: !visited;
      let neighbors = Hashtbl.find_exn g cur.pos in
      List.iter neighbors ~f:(fun neighbor ->
        Pairing_heap.add heap (PosWithDist.with_total_dist cur neighbor));
      let next = Pairing_heap.pop heap |> Option.value_exn in
      let parents_of_next = Hashtbl.find_multi parents next.pos.pos in
      (* TODO: we need to add parents if they have the same dist modulo 1-2 turns! *)
      (* If the parent is already in the list of parents to the node, don't do anythin*)
      if not
           (List.mem parents_of_next next.parent ~equal:IntPair.equal
            || IntPair.equal next.parent next.pos.pos
            || List.mem !visited next.pos ~equal:Pos.equal)
      then (
        printf "Assigning  ";
        print_s [%sexp (next.parent : IntPair.t)];
        printf " as parent to ";
        print_s [%sexp (next.pos.pos : IntPair.t)];
        Hashtbl.add_multi parents ~key:next.pos.pos ~data:next.parent;
        let new_parents = Hashtbl.find_multi parents next.pos.pos in
        print_s [%sexp (new_parents : IntPair.t list)]);
      dijkstras next cur_best)
  in
  let s = PosWithDist.{ pos = start; dist = 0; parent = start.pos } in
  Hashtbl.add_multi parents ~key:start.pos ~data:start.pos;
  let _ = dijkstras s s in
  let traverse_visited = ref [] in
  let rec traverse_path (pos : IntPair.t) count =
    let cur_parents = Hashtbl.find_multi parents pos in
    List.fold cur_parents ~init:count ~f:(fun acc parent ->
      if not (List.mem !traverse_visited parent ~equal:IntPair.equal)
      then (
        printf "(%d) " acc;
        print_s [%sexp (parent : IntPair.t)];
        traverse_path parent (acc + 1))
      else acc)
  in
  printf "Parents:\n";
  print_s [%sexp (parents : (IntPair.t, IntPair.t list) Hashtbl.t)];
  traverse_path (List.hd_exn ends).pos 1
;;

let () =
  (* Part 1 *)
  ignore shortest_path;
  (* let result = read () |> create_graph |> shortest_path in *)
  (* Part 2 *)
  let result = read () |> create_graph |> shortest_path_part2 in
  printf "Result: %d\n" result
;;
