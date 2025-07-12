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

module DiscountRegion = struct
  type t =
    { pos : Node.t
    ; area : int
    ; sides : int
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

let is_valid (pos : Node.pos) ~rows ~cols =
  0 <= pos.r && pos.r < rows && 0 <= pos.c && pos.c < cols
;;

let build_graph matrix =
  let graph = Hashtbl.create (module Node) in
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let get_neighor_in_direction get_dir (node : Node.t) =
    let dir_pos = get_dir node.pos in
    if is_valid dir_pos ~rows ~cols
       && String.(node.color = matrix.(dir_pos.r).(dir_pos.c))
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

let compute_regions_part2 matrix (graph : (Node.t, Node.t list) Base.Hashtbl.t) =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let visited = ref [] in
  let parent = Hashtbl.create (module Node) in
  let is_valid_for_matrix = is_valid ~rows ~cols in
  ignore matrix;
  let get_num_diagonals (node : Node.t) (neighbor : Node.t) : int =
    let up : Node.pos = Node.up node.pos in
    let up_r = up.r in
    let up_c = up.c in
    let up_node =
      if is_valid_for_matrix up
      then Some { Node.color = matrix.(up_r).(up_c); pos = up }
      else None
    in
    let down = Node.down node.pos in
    let down_r = down.r in
    let down_c = down.c in
    let down_node =
      if is_valid_for_matrix down
      then Some { Node.color = matrix.(down_r).(down_c); pos = down }
      else None
    in
    let left = Node.left node.pos in
    let left_r = left.r in
    let left_c = left.c in
    let left_node =
      if is_valid_for_matrix left
      then Some { Node.color = matrix.(left_r).(left_c); pos = left }
      else None
    in
    let right = Node.right node.pos in
    let right_r = right.r in
    let right_c = right.c in
    let right_node =
      if is_valid_for_matrix right
      then Some { Node.color = matrix.(right_r).(right_c); pos = right }
      else None
    in
    if Node.equal_pos neighbor.pos up
    then
      List.sum (module Int) [ left_node; right_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else if Node.equal_pos neighbor.pos down
    then
      List.sum (module Int) [ left_node; right_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else if Node.equal_pos neighbor.pos left
    then
      List.sum (module Int) [ up_node; down_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else if Node.equal_pos neighbor.pos right
    then
      List.sum (module Int) [ up_node; down_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else 0
  in
  let get_num_adjacents (node : Node.t) (neighbor : Node.t) : int =
    let up : Node.pos = Node.up node.pos in
    let down = Node.down node.pos in
    let left = Node.left node.pos in
    let right = Node.right node.pos in
    (* Neighbor dirs *)
    let up_neigh : Node.pos = Node.up neighbor.pos in
    let up_neigh_node =
      if is_valid_for_matrix up_neigh
      then Some { Node.color = matrix.(up_neigh.r).(up_neigh.c); pos = up_neigh }
      else None
    in
    let down_neigh : Node.pos = Node.down neighbor.pos in
    let down_neigh_node =
      if is_valid_for_matrix down_neigh
      then Some { Node.color = matrix.(down_neigh.r).(down_neigh.c); pos = down_neigh }
      else None
    in
    let left_neigh : Node.pos = Node.left neighbor.pos in
    let left_neigh_node =
      if is_valid_for_matrix left_neigh
      then Some { Node.color = matrix.(left_neigh.r).(left_neigh.c); pos = left_neigh }
      else None
    in
    let right_neigh : Node.pos = Node.right neighbor.pos in
    let right_neigh_node =
      if is_valid_for_matrix right_neigh
      then Some { Node.color = matrix.(right_neigh.r).(right_neigh.c); pos = right_neigh }
      else None
    in
    if Node.equal_pos neighbor.pos up
    then
      List.sum (module Int) [ left_neigh_node; right_neigh_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else if Node.equal_pos neighbor.pos down
    then
      List.sum (module Int) [ left_neigh_node; right_neigh_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else if Node.equal_pos neighbor.pos left
    then
      List.sum (module Int) [ up_neigh_node; down_neigh_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else if Node.equal_pos neighbor.pos right
    then
      List.sum (module Int) [ up_neigh_node; down_neigh_node ] ~f:(fun node_opt ->
        match node_opt with
        | Some x -> if List.mem !visited x ~equal:Node.equal then 1 else 0
        | None -> 0)
    else 0
  in
  let rec dfs (node : Node.t) (acc : DiscountRegion.t) : DiscountRegion.t =
    visited := node :: !visited;
    let neighbors = Hashtbl.find_exn graph node in
    List.fold neighbors ~init:acc ~f:(fun acc' neighbor ->
      printf "Testing neighbor: ";
      print_s [%sexp (neighbor : Node.t)];
      if not (List.mem !visited neighbor ~equal:Node.equal)
      then (
        printf "Neighbor NOT visited: currently area=%d sides=%d - " acc'.area acc'.sides;
        Hashtbl.add_exn parent ~key:neighbor ~data:node;
        print_s [%sexp (neighbor : Node.t)];
        let num_diagonals = get_num_diagonals node neighbor in
        let num_adjacents = get_num_adjacents node neighbor in
        let updated_region =
          if num_adjacents > 0 && num_diagonals > 0
          then { acc' with area = acc'.area + 1 }
          else if num_adjacents > 0
          then { acc' with area = acc'.area + 1; sides = acc'.sides - 2 }
          else if num_diagonals = 0
          then { acc' with area = acc'.area + 1 }
          else if num_diagonals = 1
          then { acc' with area = acc'.area + 1; sides = acc'.sides + 2 }
          else if num_diagonals = 2
          then { acc' with area = acc'.area + 1; sides = acc'.sides + 4 }
          else assert false
        in
        printf "Updated region:";
        print_s [%sexp (updated_region : DiscountRegion.t)];
        dfs neighbor updated_region)
      else if match Hashtbl.find parent node with
              | Some x -> Node.equal x neighbor
              | None -> false
      then (
        printf "Neighbor is parent - skipping - ";
        print_s [%sexp (neighbor : Node.t)];
        acc')
      else (
        printf "Neighbor visited: doing nothing - ";
        print_s [%sexp (neighbor : Node.t)];
        acc'))
    (* Region.{ acc' with perimeter = acc'.perimeter - 1 })) *)
  in
  (* Fold over potential starting points *)
  let regions =
    Hashtbl.fold graph ~init:[] ~f:(fun ~key:node ~data:_ acc ->
      if not (List.mem !visited node ~equal:Node.equal)
      then (
        let region = DiscountRegion.{ pos = node; area = 1; sides = 4 } in
        printf "STARTING at node with area=1 sides=4 - ";
        print_s [%sexp (node : Node.t)];
        let updated_region = dfs node region in
        updated_region :: acc)
      else acc)
  in
  print_endline "------";
  print_endline "Regions:";
  print_s [%sexp (regions : DiscountRegion.t list)];
  print_endline "------";
  regions
;;

(** 1. Build adjacency list graph of each region, and note a starting point for each type
       - node -> all neighboring nodes of same type
    2. For each starting point 2.1 area += 1; perimeter += 4 2.2 For each neighbor if
       visited: perimeter -= 1 (remove one side) else: area += 1; perimeter += 3 - 1

    Part 2: if no diagonals: add 0 sides; if one diagonal add 2 sides; if two diagonals:
    add 4 sides

    if one other adjacent; -2 sides (complete) *)
let compute_costs matrix =
  let graph = build_graph matrix in
  print_endline "------";
  print_endline "Graph:";
  print_s [%sexp (graph : (Node.t, Node.t list) Base.Hashtbl.t)];
  ignore compute_regions;
  (* compute_regions graph *)
  compute_regions_part2 matrix graph
;;

let () =
  let res =
    read ()
    |> compute_costs
    (* |> List.sum (module Int) ~f:(fun Region.{ pos = _; area; perimeter } -> *)
    (*   area * perimeter) *)
    |> List.sum (module Int) ~f:(fun DiscountRegion.{ pos = _; area; sides } ->
      area * sides)
  in
  printf "Result: %d\n" res
;;
