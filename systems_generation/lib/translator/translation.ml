open Ast.System_utils
open Int64

let get_utilization tasks =( 
  List.fold_left (fun acc task -> acc +. (float_of_int !(task.sum_wcet)) /. (float_of_int task.period)) 0.0 tasks
)

  (* UPPAAL format *)
let segement_to_rt segment = (
  let name = segment.segment_name in
  let bcet = segment.bcet in
  let wcet = segment.wcet in
  Printf.sprintf "segment %s %d %d\n" name bcet wcet
)

let task_to_rt task = (
  let res = ref "" in
  res := Printf.sprintf "task %s %d %d\n\n" task.task_name !(task.prio) task.period;
  res := List.fold_left (fun acc seg -> acc ^ (segement_to_rt seg)) !res !(task.segments);
  res := !res ^ (Printf.sprintf "\ntransition\n\tact -> %s\n" (List.hd !(task.segments)).segment_name);
  let rec transitions acc seg_list = 
    match seg_list with
    | [] -> acc
    | s1::s2::tl -> transitions (acc ^ (Printf.sprintf "\t%s -> %s\n" s1.segment_name 
                                        s2.segment_name)) (s2::tl)
    | s1 :: [] -> transitions (acc ^ (Printf.sprintf "\t%s -> end\n\n" s1.segment_name)) []
  in 
  transitions !res !(task.segments)
)

let system_to_rt system = (
  List.fold_left (fun acc task -> acc ^ (task_to_rt task)) "cores 1\n\n" system.tasks
)

let system_to_sol system = (
  List.fold_left (fun acc task -> acc ^ 
                    (Printf.sprintf "affinity_%s 0\n" task.task_name)) "" system.tasks
)

  (* SAG's utils *)
let rec pgcd a b = (
  if equal b 0L then a else pgcd b (rem a b)
)

let ppcm a b = (
  mul (div a (pgcd a b)) b
)

let get_hyper_period tasks = (
  let tsk = List.hd tasks in
  let tl = List.tl tasks in
  let hp = List.fold_left (fun acc task -> ppcm acc (of_int task.period)) 
                (of_int tsk.period) tl
  in
  if (compare hp 0L) = -1 then 
    raise (failwith "Integer overflow while computing the hyper-period.
      Please check the periods of the tasks.")
  else hp
)

  (* SAG's format *)
let rec task_to_csv task r_min r_max deadline prio acc hp acc_id= (
  (* Generates each activation until the Hyper-Period *)
  let rec segments_to_csv acc tid prio seg_list acc_id= 
    match seg_list with
    | seg::tl ->  (segments_to_csv (acc ^ (
                            Printf.sprintf "%d,%d,%Ld,%Ld,%d,%d,%Ld,%d\n" 
                                tid acc_id r_min r_max seg.bcet 
                                seg.wcet deadline prio
                  )) tid prio tl (acc_id + 1))
    | [] -> acc
  in 
  if Int64.equal r_min hp then acc 
  else 
    let per = of_int task.period in
    let tid = task.id in
    task_to_csv task 
                (add r_min per) (add r_max per) (add deadline per) prio
                (acc^(segments_to_csv "" tid prio !(task.segments) acc_id)) 
                hp (acc_id + List.length !(task.segments))  
)

let system_to_csv system  = (
  let all_prios = List.map (fun task -> !(task.prio)) system.tasks in
  let tasks = List.sort (fun t1 t2 -> Stdlib.compare !(t1.prio) !(t2.prio)) system.tasks in
  List.iter2 (fun prio task -> task.prio := prio) (
    List.sort (fun p1 p2 -> Stdlib.compare p2 p1) all_prios
  ) tasks;
  system.hyperperiod := get_hyper_period system.tasks;
  let hp = !(system.hyperperiod) in
  let rec aux acc task_list = 
    match task_list with
    | task::tl -> let deadline = of_int task.period in
                  let prio = !(task.prio) in
                  aux (acc ^ (task_to_csv task 0L 0L deadline prio "" hp 1)) tl
    | [] -> acc
  in 
  "TID , SID , RMIN , RMAX , BCET , WCET , DEADLINE , PRIO\n" ^
  aux "" system.tasks
)

  (* INFO format for parsing results *)
let system_to_info system = (
  let task_to_info task = 
    Printf.sprintf "%d %d %d\n" 
      task.id task.period (List.length !(task.segments))
  in
  let hp = !(system.hyperperiod) in
  let nb_tasks = List.length system.tasks in
  let total_seg = List.fold_left (fun acc task -> acc + List.length !(task.segments))
                       0 system.tasks 
  in
  Printf.sprintf "%Ld %d %d\n" hp nb_tasks total_seg ^
  List.fold_left (fun acc task -> acc ^ (task_to_info task)) "" system.tasks
)

  (* SY's format *)
let task_to_sy task = (
  let rec segments_to_sy acc seg_list = 
    match seg_list with
    | seg :: tl -> 
      segments_to_sy (acc ^ (Printf.sprintf "\tsegment %s %d %d %d\n"
                              seg.segment_name seg.id
                              seg.bcet seg.wcet)) tl
    | [] -> acc
  in 
  Printf.sprintf "task %s %d %d %d %d /* period => %d ms , C => %f ms , u => %f*/\n%s\n" 
    task.task_name task.id task.period 
    !(task.prio) !(task.sum_wcet) (task.period / 1000000)
    (float_of_int !(task.sum_wcet) /. 1000000.)
    (float_of_int !(task.sum_wcet) /. (float_of_int task.period))
    (segments_to_sy "" !(task.segments))
)

let system_to_sy system = (
  let rec aux acc tasks = 
    match tasks with
    | [] -> acc
    | t :: tl -> aux (acc ^ (task_to_sy t)) tl
  in
  Printf.sprintf "utilization %f\nsystem %s /* %d tasks , %d segments */\n\n%s" 
  (get_utilization system.tasks) system.system_name (List.length system.tasks) 
  (List.fold_left (fun acc t -> acc + (List.length !(t.segments))) 0 system.tasks) 
  (aux "" system.tasks)
)

  (* T's format *)
let systems_to_t systems scheduability_test =
  List.fold_left (fun acc s -> (
    if scheduability_test s.tasks then
      acc ^ (Printf.sprintf "The system %s is scheduable\n" s.system_name)
    else 
      acc ^ (Printf.sprintf "The system %s is potentially not scheduable\n" s.system_name)
  )) "" systems