open Base
open Stdio

module IntPair = struct
  type t = int * int [@@deriving sexp_of, compare, hash]
end

module Robot = struct
  type t =
    { pos : IntPair.t
    ; dir : IntPair.t
    }
  [@@deriving sexp_of]
end

module Constants = struct
  let re = Re.Perl.compile_pat "p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)"
  let h = 103
  let w = 101

  (* let h = 7 *)
  (* let w = 11 *)
  let seconds = 100
end

let read () =
  let parse_int group pos = Re.Group.get group pos |> Int.of_string in
  let parse_robot line =
    match Re.exec_opt Constants.re line with
    | Some group ->
      Robot.
        { pos = parse_int group 1, parse_int group 2
        ; dir = parse_int group 3, parse_int group 4
        }
    | None -> failwith "failed to parse robot"
  in
  In_channel.(input_lines stdin) |> List.map ~f:parse_robot
;;

let move_robot robot =
  printf "Original: ";
  print_s [%sexp (robot : Robot.t)];
  let moved_robot =
    Robot.
      { robot with
        pos =
          ( (fst robot.pos + (Constants.seconds * fst robot.dir)) % Constants.w
          , (snd robot.pos + (Constants.seconds * snd robot.dir)) % Constants.h )
      }
  in
  printf "Moved: ";
  print_s [%sexp (moved_robot : Robot.t)];
  moved_robot
;;

let count_in_quadrants robots =
  (* counts: topleft, topright, botleft, botright*)
  let counts = Array.init 4 ~f:(fun _ -> 0) in
  let left x = x < Constants.w / 2 in
  let right x = x > Constants.w / 2 in
  let up y = y < Constants.h / 2 in
  let down y = y > Constants.h / 2 in
  List.iter robots ~f:(fun r ->
    match Robot.(r.pos) with
    | x, y when left x && up y -> counts.(0) <- counts.(0) + 1
    | x, y when right x && up y -> counts.(1) <- counts.(1) + 1
    | x, y when left x && down y -> counts.(2) <- counts.(2) + 1
    | x, y when right x && down y -> counts.(3) <- counts.(3) + 1
    (* Ignore if not in quadrant *)
    | _, _ -> ());
  List.of_array counts
;;

(* Heuristic trick: christmas tree is visible once all robots are in unique positions.

   This might just be lucky but it works :) *)
let find_christmas_tree (robots : Robot.t list) : int =
  let max_loops = 1_000_000 in
  let rec loop = function
    | 0 ->
      failwith (Stdlib.Format.sprintf "no solutions found in %d iterations" max_loops)
    | remaining ->
      let seconds = max_loops - remaining in
      let moved_robots =
        List.map robots ~f:(fun r ->
          Robot.
            { r with
              pos =
                ( (fst r.pos + (seconds * fst r.dir)) % Constants.w
                , (snd r.pos + (seconds * snd r.dir)) % Constants.h )
            })
      in
      let set =
        Hash_set.of_list (module IntPair) (List.map moved_robots ~f:(fun r -> r.pos))
      in
      if Hash_set.length set = List.length moved_robots
      then seconds
      else loop (remaining - 1)
  in
  loop max_loops
;;

(*
   p=(x0, y0)   v=(v1, v2)
  (2, 4)         (2, -3)

  11 x 7
  
  (2, 4)
  (4, 1)

  ( 2 + 5 * 2 ) % 11
  (4 + 5 * (-3)) % 7
  
  12 % 1 = 1
  (4 - 15) % 7 = -11 % 7 = 3
*)
let () =
  (* p1 *)
  (* let result = *)
  (*   read () *)
  (*   |> List.map ~f:move_robot *)
  (*   |> count_in_quadrants *)
  (*   |> List.fold ~init:1 ~f:(fun acc x -> acc * x) *)
  (* in *)
  ignore move_robot;
  ignore count_in_quadrants;
  (* p2 *)
  let result = read () |> find_christmas_tree in
  printf "Result %d\n" result
;;
