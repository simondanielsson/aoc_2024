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
[@@deriving sexp_of]

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
  [@@deriving sexp, compare, hash]
end

module Point_set = Hash_set.M (Point)

let get_next_pos x y dir =
  match dir with
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Right -> x + 1, y
  | Left -> x - 1, y
;;

let get_next_state obstructions Point.{ x; y } dir =
  let open Point in
  let next_x, next_y = get_next_pos x y dir in
  printf "Next pos (%d, %d)\n" next_x next_y;
  let next_x', next_y', next_dir =
    if Hash_set.mem obstructions Point.{ x = next_x; y = next_y }
    then (
      let next_dir = get_next_dir dir in
      printf "Obstruction found at (%d, %d), turning " next_x next_y;
      print_s [%sexp (next_dir : direction)];
      let x_, y_ = get_next_pos x y next_dir in
      printf "Instead going to (%d, %d)" x_ y_;
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

let () =
  let result = read_matrix () |> fill |> count in
  printf "Result: %d" result
;;
