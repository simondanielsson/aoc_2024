open Base
open Stdio

let pattern = Re.Perl.compile_pat "(\\d+):\\s+([\\d\\s]+)"

type equation =
  { res : int
  ; ops : int list
  }
[@@deriving sexp]

let read () =
  In_channel.input_all In_channel.stdin
  |> String.split_lines
  |> List.map ~f:(fun line ->
    let group = Re.exec pattern line in
    let res = Re.Group.get group 1 |> Int.of_string in
    let ops = Re.Group.get group 2 |> String.split ~on:' ' |> List.map ~f:Int.of_string in
    { res; ops })
;;

let equation_is_ok equation =
  let res = equation.res in
  let rec aux acc operands =
    match operands with
    | hd :: tl ->
      let acc_plus = acc + hd in
      let acc_times = acc * hd in
      (* Part 2 *)
      let acc_concat = Int.to_string acc ^ Int.to_string hd |> Int.of_string in
      if aux acc_plus tl = res || aux acc_times tl = res || aux acc_concat tl = res
      then res
      else -1
    | [] -> acc
  in
  aux 0 equation.ops = res
;;

let check_equation equations =
  List.filter_map equations ~f:(fun equation ->
    if equation_is_ok equation then Some equation.res else None)
  |> List.fold ~init:0 ~f:(fun acc res -> acc + res)
;;

let () =
  let start = Stdlib.Sys.time () in
  let result = read () |> check_equation in
  printf "Result: %d: ran in %f s\n" result (Stdlib.Sys.time () -. start)
;;
