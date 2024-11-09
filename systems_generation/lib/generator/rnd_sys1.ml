open Ast.System_utils
open Seg_generator
open Tsk_generator

(* The periods are : 3, 4, 7, 9, 12, 21, 36, 84 in miliseconds *)
(* The unit used is Nanoseconds *)
let periods = [3000000; 4000000; 7000000; 9000000; 12000000; 21000000; 36000000; 84000000]

let shares = (
  let res = Hashtbl.create 8 in
  let per_sh = [
    (3000000, 1); (4000000, 5); (7000000, 2);
    (9000000, 1); (12000000, 10); (21000000, 4);
    (36000000, 2); (84000000, 15)
  ] in
  List.iter (fun (key, value) -> Hashtbl.add res key value) per_sh;
  res
  )

let gen_periods () = (
  let nb_tasks = 2 + Random.int 6 in
  get_first_n (shuffle periods) [] nb_tasks
)

let get_shares periods = (
  let rec get_shares_aux periods acc = 
    match periods with 
      | [] -> acc
      | p :: tl ->
        let new_share = try Hashtbl.find shares p
          with Not_found -> (
            Printf.printf "Error: Period %d not found in shares\n" p;
            exit 1
          )
        in
        get_shares_aux tl ((p , new_share)::acc)
  in
  get_shares_aux periods []
)

let generate_rnd_sys1 prioritize gen_wfmax edit_shares name utilization= (
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

