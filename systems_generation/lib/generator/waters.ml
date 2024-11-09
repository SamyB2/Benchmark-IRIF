open Ast.System_utils
open Seg_generator
open Tsk_generator

let core2 = ref None

let rec split list acc n = (
  if n = 0 then acc , list
  else split (List.tl list) ((List.hd list)::acc) (n - 1) 
)

(* unit -> int list * int list *)
  (* Returns a couple of periods for the core1 and core2 *)
let gen_periods1 () = (
  let periods = [1000000; 2000000; 5000000; 
    6660000; 20000000; 50000000;
    100000000; 200000000; 1000000000]
  in
  let nb_tasks = 2 + Random.int 6 in
  split (shuffle periods) [] nb_tasks
)

(* unit -> int list * int list *)
  (* Returns a couple of periods for the core1 and core2 
    With tasks: Angle_sync and Task_1000ms separated
  *)
let gen_periods2 () = (
  let periods = [1000000; 2000000; 5000000; 
    20000000; 50000000; 100000000; 200000000]
  in 
  let nb_tasks = 1 + Random.int 6 in
  let p1 , p2 = split (shuffle periods) [] nb_tasks in
  (6660000 :: p1) , (1000000000 :: p2)
)

(* int list -> int * int list *)
  (* Returns the number of segment for each period according 
    to Kramer's runnable shares *)
let get_shares periods = (
  let rec get_shares_aux periods acc = 
    match periods with 
      | [] -> acc
      | p :: tl ->
        let new_share = Hashtbl.find shares p
        in
        get_shares_aux tl ((p , new_share)::acc)
  in
  get_shares_aux periods []
)

(* string -> float -> system *)
  (* NOTE : The first time this function is called it will return the core1,
    the second time it will return the core2 *)
let generate_waters gen_periods prioritize gen_wfmax edit_shares  name utilization = (
  let rec generate_waters_aux periods = (
    let per_sh = edit_shares (get_shares periods) in
    let per_segs = gen_all_segments per_sh utilization random_wfmax in
    if per_segs = [] then generate_waters_aux periods 
    else (
      let hp = gen_wfmax 0. in 
      let task_list = pack_segments per_segs in
      prioritize task_list;
      Some {system_name = name; tasks = task_list; hyperperiod = ref (Int64.of_float hp)}
    )
  )
  in 
  match !core2 with
  | None -> (
    let per1, per2 = gen_periods () in
    let sys1 = generate_waters_aux per1 in
    core2 := generate_waters_aux per2;
    sys1
  )
  | Some sys -> (
    core2 := None;
    Some {system_name = name; tasks = sys.tasks; hyperperiod = ref 0L}
  )
)  