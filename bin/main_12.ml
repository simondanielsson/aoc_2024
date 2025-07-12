open Base
open Stdio

module Node = struct
  type pos =
    { r : int
    ; c : int
    }
  [@@deriving sexp_of, compare, hash, equal]

  type t =
    { color : string
    ; pos : pos
    }
  [@@deriving sexp_of, compare, hash, equal]

  let up pos = { c = pos.c; r = pos.r - 1 }
  let down pos = { c = pos.c; r = pos.r + 1 }
  let left pos = { c = pos.c - 1; r = pos.r }
  let right pos = { c = pos.c + 1; r = pos.r }
end

module Region = struct
  type t =
    { pos : Node.t
    ; area : int
    ; perimeter : int
    }
  [@@deriving sexp_of]
end

let read () =
  let char_matrix =
    In_channel.input_lines In_channel.stdin |> List.map ~f:String.to_list
  in
  let matrix =
    List.map char_matrix ~f:(fun lst -> List.map lst ~f:String.of_char |> Array.of_list)
    |> Array.of_list
  in
  print_endline "Matrix:";
  print_s [%sexp (matrix : string array array)];
  matrix
;;

let build_graph matrix =
  let graph = Hashtbl.create (module Node) in
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let is_valid (pos : Node.pos) =
    0 <= pos.r && pos.r < rows && 0 <= pos.c && pos.c < cols
  in
  let get_neighor_in_direction get_dir (node : Node.t) =
    let dir_pos = get_dir node.pos in
    if is_valid dir_pos && String.(node.color = matrix.(dir_pos.r).(dir_pos.c))
    then Some Node.{ color = node.color; pos = dir_pos }
    else None
  in
  let get_neighbors node =
    let up = get_neighor_in_direction Node.up node in
    let down = get_neighor_in_direction Node.down node in
    let left = get_neighor_in_direction Node.left node in
    let right = get_neighor_in_direction Node.right node in
    List.filter_opt [ up; down; left; right ]
  in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let node = Node.{ color = matrix.(r).(c); pos = { r; c } } in
      let neighbors = get_neighbors node in
      Hashtbl.add_exn graph ~key:node ~data:neighbors
    done
  done;
  graph
;;

let compute_regions (graph : (Node.t, Node.t list) Base.Hashtbl.t) =
  let visited = ref [] in
  let parent = Hashtbl.create (module Node) in
  let rec dfs (node : Node.t) (acc : Region.t) : Region.t =
    visited := node :: !visited;
    let neighbors = Hashtbl.find_exn graph node in
    List.fold neighbors ~init:acc ~f:(fun acc' neighbor ->
      printf "Testing neighbor: ";
      print_s [%sexp (neighbor : Node.t)];
      if not (List.mem !visited neighbor ~equal:Node.equal)
      then (
        printf
          "Neighbor NOT visited: adding area 1 and perimeter 2 to area=%d perimeter=%d - "
          acc'.area
          acc'.perimeter;
        Hashtbl.add_exn parent ~key:neighbor ~data:node;
        print_s [%sexp (neighbor : Node.t)];
        dfs
          neighbor
          { acc' with area = acc'.area + 1; perimeter = acc'.perimeter + 3 - 1 })
      else if match Hashtbl.find parent node with
              | Some x -> Node.equal x neighbor
              | None -> false
      then (
        printf "Neighbor is parent - skipping - ";
        print_s [%sexp (neighbor : Node.t)];
        acc')
      else (
        printf "Neighbor visited: subtracting 1 from perimeter %d - " acc'.perimeter;
        print_s [%sexp (neighbor : Node.t)];
        Region.{ acc' with perimeter = acc'.perimeter - 1 }))
  in
  (* Fold over potential starting points *)
  let regions =
    Hashtbl.fold graph ~init:[] ~f:(fun ~key:node ~data:_ acc ->
      if not (List.mem !visited node ~equal:Node.equal)
      then (
        let region = Region.{ pos = node; area = 1; perimeter = 4 } in
        printf "STARTING at node with area=1 perimeter=4 - ";
        print_s [%sexp (node : Node.t)];
        let updated_region = dfs node region in
        updated_region :: acc)
      else acc)
  in
  print_endline "------";
  print_endline "Regions:";
  print_s [%sexp (regions : Region.t list)];
  print_endline "------";
  regions
;;

(** 1. Build adjacency list graph of each region, and note a starting point for each type
       - node -> all neighboring nodes of same type
    2. For each starting point 2.1 area += 1; perimeter += 4 2.2 For each neighbor if
       visited: perimeter -= 1 (remove one side) else: area += 1; perimeter += 3 - 1 *)
let compute_costs matrix =
  let graph = build_graph matrix in
  print_endline "------";
  print_endline "Graph:";
  print_s [%sexp (graph : (Node.t, Node.t list) Base.Hashtbl.t)];
  compute_regions graph
;;

let () =
  let res =
    read ()
    |> compute_costs
    |> List.sum (module Int) ~f:(fun Region.{ pos = _; area; perimeter } ->
      area * perimeter)
  in
  printf "Result: %d\n" res
;;
