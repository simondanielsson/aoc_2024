open Base
open Stdio

let read_matrix () =
  In_channel.input_all In_channel.stdin
  |> String.split_lines
  |> List.map ~f:String.to_array
  |> Array.of_list
;;

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving sexp, compare, hash, equal]

let get_next_dir = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
;;

let get_dir = function
  | '^' -> Up
  | '<' -> Left
  | '>' -> Right
  | 'v' -> Down
  | _ -> failwith "unexpected direction"
;;

module Point = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, compare, hash, equal]
end

module PointDir = struct
  type t =
    { x : int
    ; y : int
    ; dir : direction
    }
  [@@deriving sexp, compare, hash, equal]
end

let get_next_pos x y dir =
  match dir with
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Right -> x + 1, y
  | Left -> x - 1, y
;;

let get_next_state ?(ignore_rot = false) obstructions Point.{ x; y } dir =
  let open Point in
  let next_x, next_y = get_next_pos x y dir in
  (* printf "Next pos (%d, %d)\n" next_x next_y; *)
  let next_x', next_y', next_dir =
    if Hash_set.mem obstructions Point.{ x = next_x; y = next_y } && not ignore_rot
    then (
      let next_dir = get_next_dir dir in
      (* printf "Obstruction found at (%d, %d), turning " next_x next_y; *)
      (* print_s [%sexp (next_dir : direction)]; *)
      let x_, y_ = get_next_pos x y next_dir in
      (* printf "Instead going to (%d, %d)" x_ y_; *)
      x_, y_, next_dir)
    else next_x, next_y, dir
  in
  (* Check if out of bounds *)
  { x = next_x'; y = next_y' }, next_dir
;;

let has_left_map ~h ~w obstructions Point.{ x; y } =
  if Hash_set.mem obstructions Point.{ x; y }
  then true
  else if x < 0 || x > w || y < 0 || y > h
  then true
  else false
;;

let find_obstructions marix =
  let obstructions = Hash_set.create (module Point) in
  Array.iteri marix ~f:(fun h row ->
    Array.iteri row ~f:(fun w pos ->
      if Char.equal pos '#' then Hash_set.add obstructions { x = w; y = h }));
  print_endline "Obstructions at:\n";
  print_s [%sexp (obstructions : Point.t Hash_set.t)];
  obstructions
;;

let find_guard matrix =
  let open Point in
  let is_guard value = List.mem [ '^'; '<'; '>'; 'v' ] value ~equal:Char.equal in
  let height = Array.length matrix - 1 in
  let width = Array.length matrix.(0) - 1 in
  let guard = ref ({ x = -1; y = -1 }, Up) in
  for h = 0 to height do
    for w = 0 to width do
      if is_guard matrix.(h).(w) then guard := { x = w; y = h }, get_dir matrix.(h).(w)
    done
  done;
  printf "Guard at ";
  print_s [%sexp (!guard : t * direction)];
  !guard
;;

let fill matrix =
  let open Point in
  let h = Array.length matrix - 1 in
  let w = Array.length matrix.(0) - 1 in
  let obstructions = find_obstructions matrix in
  let cur_pos_, cur_dir_ = find_guard matrix in
  let cur_pos = ref cur_pos_ in
  let cur_dir = ref cur_dir_ in
  while not (has_left_map ~h ~w obstructions !cur_pos) do
    let { x = cur_x; y = cur_y } = !cur_pos in
    matrix.(cur_y).(cur_x) <- 'X';
    let next_pos, next_dir = get_next_state obstructions !cur_pos !cur_dir in
    cur_pos := next_pos;
    cur_dir := next_dir
  done;
  print_s [%sexp (matrix : char array array)];
  matrix
;;

let count matrix =
  Array.fold matrix ~init:0 ~f:(fun acc row ->
    Array.fold row ~init:acc ~f:(fun acc' pos ->
      if Char.equal pos 'X' then acc' + 1 else acc'))
;;

(* Part 2 *)
(*
count = ref 0
For each visited position X
  insert obstruction
  pos = ref start
  visited_count = {}
  loop_found = ref false
  while not has_left_map or loop_found do
    increment visited(pos)
    pos := next_step pos
    if visited(pos) > 1000
      loop_found := true 
*)
let get_available_positions ~start matrix =
  Array.foldi matrix ~init:[] ~f:(fun h acc row ->
    Array.foldi row ~init:acc ~f:(fun w acc' pos ->
      if Char.equal pos 'X' && not (Point.equal { x = w; y = h } start)
      then Point.{ x = w; y = h } :: acc'
      else acc'))
;;

let count_loops ~original_matrix matrix =
  let open Point in
  let h = Array.length original_matrix - 1 in
  let w = Array.length original_matrix.(0) - 1 in
  let obstructions = find_obstructions original_matrix in
  let cur_pos_, cur_dir_ = find_guard original_matrix in
  (* let available_positions = get_available_positions ~start:cur_pos_ matrix in *)
  ignore matrix;
  ignore get_available_positions;
  let available_positions =
    [ Point.{ x = 6; y = 7 }; { x = 7; y = 7 }; { x = 1; y = 8 } ]
  in
  print_endline "Available positions:";
  print_s [%sexp (available_positions : t list)];
  printf "Starting position: (%d, %d)" cur_pos_.x cur_pos_.y;
  let yields_loop obstruction =
    let obstructions_copy = Hash_set.copy obstructions in
    Hash_set.add obstructions_copy obstruction;
    let visited = Hash_set.create (module PointDir) in
    let visited_count = Hashtbl.create (module Point) in
    let cur_pos = ref cur_pos_ in
    let cur_dir = ref cur_dir_ in
    let loop_found = ref false in
    while (not (has_left_map ~h ~w obstructions_copy !cur_pos)) && not !loop_found do
      let { x = cur_x__; y = cur_y__ } = !cur_pos in
      Hash_set.add visited { x = cur_x__; y = cur_y__; dir = !cur_dir };
      Hashtbl.update visited_count !cur_pos ~f:(function
        | Some x -> x + 1
        | None -> 1);
      (* printf *)
      (*   "Current visited count of (%d, %d) is %d\n" *)
      (*   !cur_pos.x *)
      (*   !cur_pos.y *)
      (*   (Hashtbl.find visited_count !cur_pos |> Option.value ~default:(-1)); *)
      let next_pos, next_dir = get_next_state obstructions_copy !cur_pos !cur_dir in
      cur_pos := next_pos;
      cur_dir := next_dir;
      let { x = cur_x___; y = cur_y___ } = !cur_pos in
      let inverse_pointdir =
        PointDir.{ x = cur_x___; y = cur_y___; dir = get_next_dir !cur_dir }
      in
      let next, _ = get_next_state ~ignore_rot:true obstructions_copy !cur_pos !cur_dir in
      printf
        "In: (%d, %d); Next: (%d, %d) would be obstacle %s\n"
        !cur_pos.x
        !cur_pos.y
        next.x
        next.y
        (Hash_set.mem obstructions_copy next |> Bool.to_string);
      if
        (Hash_set.mem visited inverse_pointdir && Hash_set.mem obstructions_copy next)
        || Hashtbl.find visited_count !cur_pos |> Option.value ~default:0 > 5
      then (
        printf "Found loop by inserting at (%d, %d)\n" obstruction.x obstruction.y;
        print_s [%sexp (inverse_pointdir : PointDir.t)];
        print_s [%sexp (!cur_dir : direction)];
        print_s [%sexp (next : t)];
        printf
          "next would be obstacle: %s\n"
          (Hash_set.mem obstructions_copy next |> Bool.to_string);
        loop_found := true)
    done;
    !loop_found
  in
  List.fold available_positions ~init:0 ~f:(fun acc obstruction ->
    if yields_loop obstruction then acc + 1 else acc)
;;

let () =
  (* Part 1 *)
  (* let result_part1 = read_matrix () |> fill |> count in *)
  (* printf "Result: %d" result_part1; *)
  ignore count;
  (* Part 2 *)
  let matrix = read_matrix () in
  let original_matrix = Array.copy_matrix matrix in
  let result_part2 = fill matrix |> count_loops ~original_matrix in
  printf "Result: %d\n" result_part2
;;
