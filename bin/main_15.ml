open Base
open Stdio

module Entity = struct
  type t =
    | Robot
    | Wall
    | Box
    | Empty
  [@@deriving sexp_of, equal]

  let of_char = function
    | '@' -> Robot
    | '#' -> Wall
    | 'O' -> Box
    | '.' -> Empty
    | x -> failwith (Stdlib.Format.sprintf "invalid symbol: %c" x)
  ;;
end

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of]

  let of_char = function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | x -> failwith (Stdlib.Format.sprintf "invalid symbol: %c" x)
  ;;

  let next_pos t (r, c) =
    match t with
    | Up -> r - 1, c
    | Down -> r + 1, c
    | Left -> r, c - 1
    | Right -> r, c + 1
  ;;
end

let read () =
  let lines = In_channel.(input_lines stdin) in
  let split_index, _ = List.findi_exn lines ~f:(fun _ line -> String.( = ) line "") in
  let map_str, dirs_str = List.partitioni_tf lines ~f:(fun i _ -> i < split_index) in
  let robot_pos = ref (-1, -1) in
  let create_map lines =
    let rows = List.length lines in
    let cols = String.length (List.hd_exn lines) in
    let map = Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> Entity.Empty)) in
    List.iteri lines ~f:(fun r s ->
      String.iteri s ~f:(fun c value ->
        let entity = Entity.of_char value in
        map.(r).(c) <- entity;
        match entity with
        | Robot -> robot_pos := r, c
        | _ -> ()));
    map
  in
  let create_dirs lines =
    String.concat lines |> String.to_sequence |> Sequence.map ~f:Dir.of_char
  in
  (* First element of the dirs is the empty string *)
  let map = create_map map_str in
  let dirs = create_dirs (List.tl_exn dirs_str) in
  map, dirs, !robot_pos
;;

let move (map, dirs, robot_pos) =
  print_s [%sexp (map : Entity.t array array)];
  print_s [%sexp (dirs : Dir.t Sequence.t)];
  printf "Robot at (%d. %d)\n" (fst robot_pos) (snd robot_pos);
  let robot = ref robot_pos in
  let rec try_move dir (r, c) : bool =
    printf "From: (%d, %d) moving %s\n" r c (Dir.sexp_of_t dir |> Sexp.to_string);
    let r', c' = Dir.next_pos dir (r, c) in
    match map.(r').(c') with
    | Empty ->
      (* Ok, move was valid. Move from old to new. *)
      map.(r').(c') <- map.(r).(c);
      true
    | Wall ->
      (* Hit a wall, discard *)
      false
    | Box ->
      (* See if we can move this box *)
      if try_move dir (r', c')
      then (
        map.(r').(c') <- map.(r).(c);
        true)
      else false
    | Robot -> failwith "unreachable state: found robot"
  in
  Sequence.iter dirs ~f:(fun dir ->
    let valid_move = try_move dir !robot in
    printf "Valid move: %b\n" valid_move;
    if valid_move
    then (
      let r, c = !robot in
      map.(r).(c) <- Empty;
      robot := Dir.next_pos dir !robot);
    ());
  print_endline "After moving:";
  print_s [%sexp (map : Entity.t array array)];
  map
;;

let calculate_score map =
  let find_boxes map =
    Array.foldi map ~init:[] ~f:(fun r acc row ->
      Array.foldi row ~init:acc ~f:(fun c acc' value ->
        if Entity.equal value Box then (r, c) :: acc' else acc'))
  in
  let score_box (r, c) = (100 * r) + c in
  find_boxes map |> List.sum (module Int) ~f:score_box
;;

let () =
  let result = read () |> move |> calculate_score in
  printf "Result: %d\n" result
;;
