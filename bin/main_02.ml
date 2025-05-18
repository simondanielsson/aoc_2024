open Base
open Stdio

(* Part 1 *)
let is_pair_safe ~is_ascending fst snd =
  if is_ascending
  then snd > fst && snd - fst >= 1 && snd - fst <= 3
  else snd < fst && fst - snd >= 1 && fst - snd <= 3
;;

let is_safe report =
  let is_ascending =
    match report with
    | fst :: snd :: _ -> snd > fst
    | _ :: [] | [] -> true
  in
  let rec levels_are_safe = function
    | fst :: snd :: _ as remainder ->
      is_pair_safe ~is_ascending fst snd
      && levels_are_safe (List.tl remainder |> Option.value ~default:[])
    | _ :: [] | [] -> true
  in
  levels_are_safe report
;;

(* Part 2 *)
let is_safe_with_tolerance report =
  let already_safe = is_safe report in
  let is_fixable_with_removal =
    let indices = List.init (List.length report) ~f:Fn.id in
    let reports_without_index =
      List.map indices ~f:(fun index ->
        List.filteri report ~f:(fun idx _ -> idx <> index))
    in
    List.fold reports_without_index ~init:false ~f:(fun acc report_without_index ->
      acc || is_safe report_without_index)
  in
  already_safe || is_fixable_with_removal
;;

let count_safe_reports ~safety_fun =
  let rec aux acc =
    match In_channel.input_line In_channel.stdin with
    | None | Some "" -> acc
    | Some value ->
      let levels = String.split ~on:' ' value |> List.map ~f:Int.of_string in
      aux (if safety_fun levels then acc + 1 else acc)
  in
  aux 0
;;

let run_part1 () =
  let safe_count = count_safe_reports ~safety_fun:is_safe in
  printf "Safe count: %d\n" safe_count
;;

let run_part2 () =
  let safe_count_with_tol = count_safe_reports ~safety_fun:is_safe_with_tolerance in
  printf "Safe count with tolerance: %d\n" safe_count_with_tol
;;

let () =
  ignore run_part1;
  run_part2 ()
;;
