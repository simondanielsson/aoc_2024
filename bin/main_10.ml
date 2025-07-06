open Base
open Stdio

module Pos = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp_of, equal]

  let up r c = { x = c; y = r - 1 }
  let down r c = { x = c; y = r + 1 }
  let left r c = { x = c - 1; y = r }
  let right r c = { x = c + 1; y = r }

  let is_rise matrix t value =
    let local_ rows = Array.length matrix in
    let local_ cols = Array.length matrix.(0) in
    if 0 <= t.y && t.y < rows && 0 <= t.x && t.x < cols
    then matrix.(t.y).(t.x) = value + 1
    else false
  ;;
end

let read () =
  let char_matrix =
    In_channel.input_lines In_channel.stdin |> List.map ~f:String.to_list
  in
  let matrix =
    List.map char_matrix ~f:(fun lst ->
      List.map lst ~f:(fun x -> String.of_char x |> Int.of_string) |> Array.of_list)
    |> Array.of_list
  in
  print_endline "Matrix:";
  print_s [%sexp (matrix : int array array)];
  matrix
;;

(** [find_trailheads] finds all all positions starting at height 0 *)
let find_trailheads matrix =
  let trailheads = ref [] in
  for row = 0 to Array.length matrix - 1 do
    for col = 0 to Array.length matrix.(0) - 1 do
      if matrix.(row).(col) = 0 then trailheads := Pos.{ x = col; y = row } :: !trailheads
    done
  done;
  print_endline "Trailheads:";
  print_s [%sexp (!trailheads : Pos.t list)];
  !trailheads
;;

let create_graph matrix =
  let local_ rows = Array.length matrix in
  let local_ cols = Array.length matrix.(0) in
  let graph = Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> ref [])) in
  Array.iteri matrix ~f:(fun r row ->
    Array.iteri row ~f:(fun c value ->
      let up = Pos.up r c in
      let down = Pos.down r c in
      let left = Pos.left r c in
      let right = Pos.right r c in
      if Pos.is_rise matrix up value then graph.(r).(c) := up :: !(graph.(r).(c));
      if Pos.is_rise matrix down value then graph.(r).(c) := down :: !(graph.(r).(c));
      if Pos.is_rise matrix left value then graph.(r).(c) := left :: !(graph.(r).(c));
      if Pos.is_rise matrix right value then graph.(r).(c) := right :: !(graph.(r).(c))));
  print_endline "Graph:";
  print_s [%sexp (graph : Pos.t list ref array array)];
  graph
;;

let compute_score matrix (trailhead : Pos.t) (graph : Pos.t list ref array array) =
  (* To a DFS until we reach a 9 node, then add 1 *)
  let visited : Pos.t list ref = ref [] in
  let rec loop Pos.{ x; y } acc =
    if matrix.(y).(x) = 9
    then (
      printf "(%d, %d) 9 found\n" y x;
      acc + 1)
    else (
      let neighbors = graph.(y).(x) in
      List.fold !neighbors ~init:acc ~f:(fun acc' neighbor ->
        if not (List.mem !visited neighbor ~equal:Pos.equal)
        then (
          let Pos.{ x = x'; y = y' } = neighbor in
          printf "Visited (%d, %d)\n" y' x';
          visited := neighbor :: !visited;
          loop neighbor acc')
        else acc'))
  in
  loop trailhead 0
;;

let create_graph_and_compute_scores matrix =
  let trailheads = find_trailheads matrix in
  let graph = create_graph matrix in
  List.fold trailheads ~init:[] ~f:(fun acc trailhead ->
    compute_score matrix trailhead graph :: acc)
;;

let sum numbers =
  print_s [%sexp (numbers : int list)];
  List.fold numbers ~init:0 ~f:(fun acc x -> acc + x)
;;

let () =
  let result = read () |> create_graph_and_compute_scores |> sum in
  printf "Result: %d\n" result
;;
