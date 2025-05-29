open Base
open Stdio

let read () =
  In_channel.input_all In_channel.stdin
  |> String.split_lines
  |> List.map ~f:String.to_array
  |> Array.of_list
;;

(*
   Initialize empty count matrix of same size with zeros everywhere
Identify positions of each type of freq
A: [(1,2 ), (3,4 )]

For each frequency
  For each pair 
    node positions = calculate node positions within matrix
    for each node position
      increment count matrix at position

sum up the count matrix
*)

type position =
  { x : int
  ; y : int
  }
[@@deriving sexp]

let find_antennas matrix =
  let matrix_lst = Array.map matrix ~f:List.of_array |> List.of_array |> List.concat in
  let frequencies = Hash_set.of_list (module Char) matrix_lst in
  Hash_set.remove frequencies '.';
  Hash_set.fold frequencies ~init:[] ~f:(fun acc freq ->
    let freq_acc = ref [] in
    Array.iteri matrix ~f:(fun h row ->
      Array.iteri row ~f:(fun w cell ->
        if Char.equal cell freq then freq_acc := { x = w; y = h } :: !freq_acc));
    !freq_acc :: acc)
;;

(* [ [ { x = 8; y = 1 }; { x = 5; y = 2 } ] ] *)

let rec find_nodes_part1 acc antennas =
  match antennas with
  | hd :: tl ->
    (* construct all combinations of hd and tl[i]*)
    (* Find the node positions of each combination *)
    let nodes =
      List.fold tl ~init:[] ~f:(fun node_acc next ->
        let x_diff = next.x - hd.x in
        let y_diff = next.y - hd.y in
        printf "x_diff=%d, y_diff=%d\n" x_diff y_diff;
        let first_node = { x = hd.x - x_diff; y = hd.y - y_diff } in
        let second_node = { x = next.x + x_diff; y = next.y + y_diff } in
        first_node :: second_node :: node_acc)
    in
    let acc' = List.fold nodes ~init:acc ~f:(fun acc' node -> node :: acc') in
    find_nodes_part1 acc' tl
  | [] -> acc
;;

let count_antinodes_part1 matrix =
  let width = Array.length matrix.(0) in
  let height = Array.length matrix in
  let count_matrix = Array.make_matrix ~dimx:width ~dimy:height 0 in
  let antenna_positions = find_antennas matrix in
  print_s [%sexp (antenna_positions : position list list)];
  List.iter antenna_positions ~f:(fun antennas ->
    let nodes = find_nodes_part1 [] antennas in
    print_s [%sexp (nodes : position list)];
    List.iter nodes ~f:(fun { x = w; y = h } ->
      if 0 <= h && h <= height - 1 && 0 <= w && w <= width - 1
      then count_matrix.(h).(w) <- 1));
  print_s [%sexp (count_matrix : int array array)];
  Array.fold count_matrix ~init:0 ~f:(fun acc row ->
    Array.fold row ~init:acc ~f:(fun acc' cell -> acc' + cell))
;;

(* Part 2 *)
let rec find_nodes_part2 ~height ~width acc antennas =
  match antennas with
  | hd :: tl ->
    (* construct all combinations of hd and tl[i]*)
    (* Find the node positions of each combination *)
    let nodes =
      List.foldi tl ~init:[] ~f:(fun i node_acc next ->
        let x_diff = next.x - hd.x in
        let y_diff = next.y - hd.y in
        let n = ref 0 in
        let nodes = ref [] in
        let fst_x = ref (hd.x - (!n * x_diff)) in
        let fst_y = ref (hd.y - (!n * y_diff)) in
        let snd_x = ref (next.x + (!n * x_diff)) in
        let snd_y = ref (next.y + (!n * y_diff)) in
        while
          (0 <= !fst_x && !fst_x <= width && 0 <= !fst_y && !fst_y <= height)
          || (0 <= !snd_x && !snd_x <= width && 0 <= !snd_y && !snd_y <= height)
        do
          printf "%d: n=%d\n" i !n;
          let first_node = { x = !fst_x; y = !fst_y } in
          let second_node = { x = !snd_x; y = !snd_y } in
          Int.incr n;
          fst_x := hd.x - (!n * x_diff);
          fst_y := hd.y - (!n * y_diff);
          snd_x := next.x + (!n * x_diff);
          snd_y := next.y + (!n * y_diff);
          printf "%d: (%d, %d) - (%d, %d)\n" i !fst_x !fst_y !snd_x !snd_y;
          nodes := first_node :: second_node :: !nodes
        done;
        !nodes @ node_acc)
    in
    let acc' = List.fold nodes ~init:acc ~f:(fun acc' node -> node :: acc') in
    find_nodes_part2 ~height ~width acc' tl
  | [] -> acc
;;

let count_antinodes_part2 matrix =
  let width = Array.length matrix.(0) in
  let height = Array.length matrix in
  let count_matrix = Array.make_matrix ~dimx:width ~dimy:height 0 in
  let antenna_positions = find_antennas matrix in
  print_s [%sexp (antenna_positions : position list list)];
  List.iter antenna_positions ~f:(fun antennas ->
    match antennas with
    | [ { x; y } ] ->
      (* Add increment count on the antenna position *)
      count_matrix.(y).(x) <- 1
    | _ as antennas ->
      let nodes = find_nodes_part2 ~height ~width [] antennas in
      print_s [%sexp (nodes : position list)];
      List.iter nodes ~f:(fun { x = w; y = h } ->
        if 0 <= h && h <= height - 1 && 0 <= w && w <= width - 1
        then count_matrix.(h).(w) <- 1));
  print_s [%sexp (count_matrix : int array array)];
  Array.fold count_matrix ~init:0 ~f:(fun acc row ->
    Array.fold row ~init:acc ~f:(fun acc' cell -> acc' + cell))
;;

let () =
  let start = Stdlib.Sys.time () in
  (* let result = read () |> count_antinodes_part1 in *)
  ignore count_antinodes_part1;
  let result = read () |> count_antinodes_part2 in
  printf "Result: %d: ran in %f s\n" result (Stdlib.Sys.time () -. start)
;;
