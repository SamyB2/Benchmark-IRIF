open Ast.System_utils

let shares = (
  let aux () = let res = Hashtbl.create 9 in
    let per_sh = [
      (1000000, 3); (2000000, 2); (5000000, 2);
      (6660000, 15); (10000000, 25); (20000000, 25);
      (50000000, 3); (100000000, 20); (200000000, 1);
      (1000000000, 4);
    ] in
    List.iter (fun (key, value) -> Hashtbl.add res key value) per_sh;
    res
  in aux ()
)

let random_shares per_sh = (
  let max_sh = 10 + List.fold_left (fun acc (_, v) -> max acc v) 0 per_sh in
  List.map (fun (p, _) -> p, (1 + Random.int max_sh)) per_sh
)

let fixed_shares per_sh = per_sh

let acets = (
  let aux () = let res = Hashtbl.create 17 in 
    let per_acet = [
      (1000000, 5000.); (2000000, 4200.); (3000000, 3500.);
      (4000000, 5500.); (5000000, 11040.);(6660000, 6520.);
      (7000000, 4300.); (9000000, 10000.); (10000000, 10090.);
      (12000000, 13560.); (20000000, 8740.); (21000000, 12680.);
      (36000000, 1954.); (50000000, 17560.); (84000000, 1962.);
      (100000000, 10530.); (200000000, 2560.); (1000000000, 430.);
    ] in
    List.iter (fun (key, value) -> Hashtbl.add res key value) per_acet;
    res
  in aux ()
)

let random_wfmax wfmax = (
  if wfmax < 15. then wfmax
  else
    let p = Random.int 100 in 
      match p with
        | _ when p < 35 -> wfmax /. (
          if wfmax > 19. && wfmax < 20. 
            then 11. 
          else 14.)
        | _ when p < 70 -> wfmax /. 10.
        | _ when p < 80 -> wfmax /. 5.
        | _ when p < 90 -> wfmax /. 2.
        | _ -> wfmax 
)

let fixed_wfmax wfmax = wfmax

let factors = (
  let aux () = let res = Hashtbl.create 17 in
  let data = [
    (1000000, ((0.19, 0.92), (1.3, 29.11)));
    (2000000, ((0.12, 0.89), (1.54, 19.04)));
    (3000000, ((0.15, 0.95), (1.12, 19.63)));
    (4000000, ((0.14, 0.93), (1.18, 21.43)));
    (5000000, ((0.17, 0.94), (1.13, 18.44)));
    (6660000, ((0.13, 0.92), (1.20, 28.17)));
    (7000000, ((0.07, 0.93), (1.34, 22.67)));
    (9000000, ((0.15, 0.94), (1.26, 31.67)));
    (10000000, ((0.05, 0.99), (1.06, 30.03)));
    (12000000, ((0.12, 0.97), (1.08, 26.27)));
    (20000000, ((0.11, 0.98), (1.06, 15.61)));
    (21000000, ((0.14, 0.96), (1.09, 16.61)));
    (36000000, ((0.32, 0.97), (1.13, 9.92)));
    (50000000, ((0.32, 0.95), (1.13, 7.45)));
    (84000000, ((0.32, 0.96), (1.13, 14.76)));
    (100000000, ((0.09, 0.99), (1.02, 8.88)));
    (200000000, ((0.45, 0.98), (1.03, 4.9)));
    (1000000000, ((0.68, 0.80), (1.84, 4.75)));
  ] in
  List.iter (fun (key, value) -> Hashtbl.add res key value) data;
  res
  in aux ()
)

(*  unit -> (int, 'a list * int) Hashtbl *)
  (* Returns a Hashtbl where the keys are the periods. *)
  (* The values are a couple where the first element is 
    the segments associated to the period and the second
    it's wcet *)
let init_hash per_sh = (
  let res = Hashtbl.create (List.length per_sh) in
  List.iter (fun (p, _) -> Hashtbl.add res p ([], 0)) per_sh;
  res
)

let generate_segment id period gen_wfmax = (
  let generate_bcet_wcet avg = 
    let (bfmin, bfmax), (wfmin, wfmax) = try Hashtbl.find factors period 
  with Not_found -> (
    Printf.printf "Error: Period %d not found in factors\n" period;
    exit 1
  )
  in
    let min_bcet = int_of_float (bfmin *. avg) in
    let max_bcet = int_of_float (bfmax *. avg) in
    let max_wcet = int_of_float ((gen_wfmax wfmax) *. avg) in
    let min_wcet = int_of_float (wfmin *. avg) in
    (min_bcet + Random.int (max_bcet - min_bcet)), (min_wcet + Random.int (max_wcet - min_wcet))
  in
  let name = "seg_" ^ (string_of_int id) in
  let avg = try Hashtbl.find acets period 
with Not_found -> (
  Printf.printf "Error: Period %d not found in acets\n" period;
  exit 1
)
in
  let bcet, wcet = generate_bcet_wcet avg in
  {segment_name = name; id = id;
    bcet = bcet; wcet = wcet}
)

let generate_segment_list id period nb gen_wfmax = (
  let rec aux acc id nb sum_wcet=
    if nb = 0 then (List.rev acc) , sum_wcet
    else 
      let seg = generate_segment id period gen_wfmax in
      aux (seg::acc) (id + 1) (nb - 1) (sum_wcet + seg.wcet)
  in aux [] id nb 0
)

(* int * int list -> float -> int * (segement list) list *)
  (* Generates the segments for each period and returns the
    association list *)
let gen_all_segments per_sh utilization gen_wfmax = (
  let compute_utilization per_seg = (
    Hashtbl.fold (fun k v acc -> acc +. ((float_of_int (snd v)) /. (float_of_int k)))
      per_seg 0.
  )
  in
  let per_seg = init_hash per_sh in
  (* Generates the appropriate number of segments for each period *)
    (* It updates the local variable `per_seg` by adding to it the new segments *)
  let rec gen_full_set per_sh factor = (
    match per_sh with
    | [] -> ()
    | (p, sh) :: tl -> 
      let new_segs , new_wcet = generate_segment_list (factor*sh +  1) p sh gen_wfmax in
      let last_segs , last_wcet = Hashtbl.find per_seg p in
      Hashtbl.replace per_seg p (last_segs @ new_segs , last_wcet + new_wcet);
      gen_full_set tl factor
  )
  in
  (* It generates the adequate number of segments for the given utilization *)
    (* Returns true if it was possible false otherwise *)
  let rec gen_maximum_segments nb = (
    let u = compute_utilization per_seg in 
    if u > (utilization +. 0.01) then false
    else if u >= utilization -. 0.01 then true
    else (
      gen_full_set per_sh nb ;
      gen_maximum_segments (nb + 1)
    )
  ) 
  in
  if not (gen_maximum_segments 0) then [] else 
    let p_s = Hashtbl.fold (fun k v acc -> if (fst v) = [] then acc else (k, fst v)::acc) per_seg []
    in List.sort (fun (p1, _) (p2, _) -> compare p1 p2) p_s
)
