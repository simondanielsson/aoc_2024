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

module Pos = struct
  type t =
    { pos : int * int
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
    }
  [@@deriving sexp_of]

  let from_pair (cur : t) (neighbor : Pos.t) =
    if Dir.equal cur.pos.dir neighbor.dir
    then { pos = neighbor; dist = cur.dist + Constants.straight_weight }
    else { pos = neighbor; dist = cur.dist + Constants.turn_weight }
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

  let isempty = function
    | Empty | Start | End -> true
    | _ -> false
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
      if within_bounds r' c' && PosType.isempty matrix.(r').(c')
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

let shortest_path (g, start, ends) =
  (* print_endline "Graph:"; *)
  (* print_s [%sexp (g : graph)]; *)
  (* print_endline "Start and end:"; *)
  (* print_s [%sexp (start : Pos.t)]; *)
  (* print_s [%sexp (ends : Pos.t list)]; *)
  let visited = ref [] in
  let heap = Pairing_heap.create ~cmp:PosWithDist.compare_dist () in
  let rec dijkstras (cur : PosWithDist.t) =
    if List.mem !visited cur.pos ~equal:Pos.equal
    then (
      (* print_endline "Already visited!"; *)
      let next = Pairing_heap.pop heap |> Option.value_exn in
      dijkstras next)
    else if List.exists ends ~f:(fun _end -> Pos.same_pos cur.pos _end)
    then cur.dist
    else (
      visited := cur.pos :: !visited;
      let neighbors = Hashtbl.find_exn g cur.pos in
      List.iter neighbors ~f:(fun neighbor ->
        Pairing_heap.add heap (PosWithDist.from_pair cur neighbor));
      let next = Pairing_heap.pop heap |> Option.value_exn in
      dijkstras next)
  in
  dijkstras PosWithDist.{ pos = start; dist = 0 }
;;

let () =
  let result = read () |> create_graph |> shortest_path in
  printf "Result: %d\n" result
;;
