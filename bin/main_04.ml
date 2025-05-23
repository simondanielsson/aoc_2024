open Base
open Stdio

let read () = In_channel.input_all In_channel.stdin |> String.split_lines

let count_xmas rows =
  let matrix = List.map rows ~f:(fun s -> String.to_array s) |> Array.of_list in
  let t_matrix = Array.transpose_exn matrix in
  let height = Array.length matrix in
  let width = Array.length matrix.(0) in
  let xmas = Hash_set.of_list (module String) [ "XMAS"; "SAMX" ] in
  let count = ref 0 in
  (* print_s [%sexp (matrix : char array array)]; *)
  (* print_s [%sexp (t_matrix : char array array)]; *)
  printf "%d %d\n" height width;
  (* Check rows *)
  for h = 0 to height - 1 do
    for w = 0 to width - 4 do
      if Hash_set.mem xmas (String.of_array (Array.sub matrix.(h) ~pos:w ~len:4))
      then (
        printf "Found in w=%d h=%d\n" w h;
        count := !count + 1)
    done
  done;
  printf "In rows: %d\n" !count;
  let rowcount = !count in
  (* Check cols *)
  for w = 0 to width - 1 do
    for h = 0 to height - 4 do
      if Hash_set.mem xmas (String.of_array (Array.sub t_matrix.(w) ~pos:h ~len:4))
      then (
        printf "Found in w=%d h=%d\n" w h;
        count := !count + 1)
    done
  done;
  let colcount = !count - rowcount in
  printf "In cols: %d\n" colcount;
  (* Diagonals down *)
  for h = 0 to height - 4 do
    for w = 0 to width - 4 do
      if
        Hash_set.mem
          xmas
          (String.of_array
             [| matrix.(h).(w)
              ; matrix.(h + 1).(w + 1)
              ; matrix.(h + 2).(w + 2)
              ; matrix.(h + 3).(w + 3)
             |])
      then (
        printf "Found starting in w=%d h=%d\n" w h;
        count := !count + 1)
    done
  done;
  (* Diagonals up *)
  for h = 3 to height - 1 do
    for w = 0 to width - 4 do
      if
        Hash_set.mem
          xmas
          (String.of_array
             [| matrix.(h).(w)
              ; matrix.(h - 1).(w + 1)
              ; matrix.(h - 2).(w + 2)
              ; matrix.(h - 3).(w + 3)
             |])
      then (
        printf "Found starting in w=%d h=%d\n" w h;
        count := !count + 1)
    done
  done;
  let diagcount = !count - rowcount - colcount in
  printf "In diags: %d\n" diagcount;
  !count
;;

let is_xmas ~mas matrix h w =
  let down_diag =
    String.of_array [| matrix.(h).(w); matrix.(h + 1).(w + 1); matrix.(h + 2).(w + 2) |]
  in
  let up_diag =
    String.of_array [| matrix.(h + 2).(w); matrix.(h + 1).(w + 1); matrix.(h).(w + 2) |]
  in
  Hash_set.mem mas down_diag && Hash_set.mem mas up_diag
;;

let count_xmas_part_2 rows =
  let matrix = List.map rows ~f:(fun s -> String.to_array s) |> Array.of_list in
  let height = Array.length matrix in
  let width = Array.length matrix.(0) in
  let count = ref 0 in
  let mas = Hash_set.of_list (module String) [ "MAS"; "SAM" ] in
  for h = 0 to height - 3 do
    for w = 0 to width - 3 do
      if is_xmas ~mas matrix h w
      then
        (* printf "Found in h=%d w=%d\n" h w; *)
        count := !count + 1
    done
  done;
  !count
;;

let () =
  ignore count_xmas;
  let counts = read () |> count_xmas_part_2 in
  printf "Counts: %d\n" counts
;;
