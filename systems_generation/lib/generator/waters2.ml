open Ast.System_utils
open Seg_generator
open Tsk_generator

let gen_periods () = (
  [2000000; 5000000; 20000000;
    50000000; 100000000;
    200000000; 1000000000]
)

let get_shares periods = (
  let shares = [1; 1; 14; 2; 11; 1; 2] in 
  List.combine periods shares
)

let generate_waters2 prioritize gen_wfmax edit_shares name utilization= (
  let periods = gen_periods () in 
  let per_sh = edit_shares (get_shares periods) in
  let per_segs = gen_all_segments per_sh utilization gen_wfmax in
  if per_segs = [] then None 
  else (
    let task_list = pack_segments per_segs in
    prioritize task_list;
    Some {system_name = name; tasks = task_list; hyperperiod = ref 0L}
  )
)  