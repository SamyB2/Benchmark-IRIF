open Ast.System_utils
open Seg_generator
open Tsk_generator

let periods = [1000000; 2000000; 5000000; 10000000;
20000000; 50000000; 100000000;
200000000; 1000000000]

(* Returns a list of periods according to `nb_tasks` *)
let gen_periods () = (
  let nb_tasks = 2 + Random.int 7 in
  get_first_n (shuffle periods) [] nb_tasks
)
(* int list -> int * int list *)
  (* Returns the number of segment for each period according 
    to Kramer's runnable shares *)
  (* If there is any missing period we sum to the back *)
let get_shares periods = (
  let rec get_total_shares periods acc last_per = 
    match periods with 
      | [] -> acc
      | p :: [] -> 
        let new_share = Hashtbl.fold (
          fun k v acc -> if k > last_per then acc + v else acc
          ) shares 0
        in 
        (p , new_share)::acc
      | p :: tl ->
        let new_share = Hashtbl.fold (
          fun k v acc -> if k <= p && k > last_per then acc + v else acc
          ) shares 0
        in
        get_total_shares tl ((p , new_share)::acc) p
  in
  let total_shares = List.sort (fun (_, s1) (_, s2) -> compare s1 s2)
    (get_total_shares periods [] 0) in 
  let min_share = snd (List.hd total_shares) in
  List.map (fun (p, s) -> p, (s / min_share)) total_shares
)

(* Returns a task list using the algorithm stated in M.Nasri and B.Brandenburg paper *)
let pack_threshold (per_segs : (int * segment list) list) = (
  let rec check_threshold period segments acc_res acc_s sum_wcet a_max = (
    let a = Random.int a_max in
    match segments with
    | [] -> List.rev (if acc_s = [] then acc_res else (List.rev acc_s) :: acc_res)
    | s :: tl -> 
      if sum_wcet + s.wcet > a then 
        if acc_s = [] then check_threshold period segments acc_res [] 0 a_max
        else check_threshold period segments ((List.rev acc_s)::acc_res) [] 0 a_max
      else check_threshold period tl acc_res (s::acc_s) (sum_wcet + s.wcet) a_max  
        
    )
  in
  (* task list -> int -> (segement list) list -> int -> task list *)
  let rec segs_to_task acc nb segs period = (
    match segs with 
    | [] -> List.rev acc
    | s :: tl -> 
      let sum_wcet = List.fold_left (fun acc s -> acc + s.wcet) 0 s in 
      segs_to_task ((generate_task nb period s sum_wcet) :: acc) (nb +1) tl period
  ) 
  in
  let t1 = fst (List.hd per_segs) in
  let c1 = (List.fold_left (fun acc s -> acc + s.wcet) 0 (snd (List.hd per_segs))) in
  let a = (2 * (t1 - c1)) in
  let first_task = generate_task 1 t1 (snd (List.hd per_segs)) c1 in
  let rec aux acc per_seg = (
    match per_seg with
    | [] -> List.rev acc
    | (p, segs) :: tl ->
      let new_segs = check_threshold p segs [] [] 0 a in
      let tasks = segs_to_task [] (List.length acc + 1) new_segs p in
      aux (acc @ tasks) tl
  )
  in List.rev (aux [first_task] (List.tl per_segs))
)

let generate_waters_sag prioritize gen_wfmax edit_shares name utilization= (
  let periods = gen_periods () in
  let per_sh = edit_shares (get_shares periods) in
  let per_segs = gen_all_segments per_sh utilization gen_wfmax in
  if per_segs = [] then None 
  else (
    let task_list = pack_threshold per_segs in
    if List.length task_list > 8 then None
    else(
      prioritize task_list;
      Some {system_name = name; tasks = task_list; hyperperiod = ref 0L}
    )
  )
)