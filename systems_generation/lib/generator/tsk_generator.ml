open Ast.System_utils

let generate_task id period segs s_wcet= (
  let name = "task_" ^ (string_of_int id) in
  let prio = ref 0 in
  let segment_list = ref segs in
  let sum_wcet = ref s_wcet in
  {task_name = name; id = id;
    prio = prio; period = period;
    segments = segment_list; sum_wcet = sum_wcet}
)

let shuffle list = (
  let nd = List.map (fun c -> (Random.bits (), c)) list in
  let sond = List.sort compare nd in
  List.map snd sond
)

let rec get_first_n list acc n = (
  if n = 0 then List.sort compare acc
  else get_first_n (List.tl list) ((List.hd list)::acc) (n - 1)
)

(* task list -> int list -> unit *)
  (* Sets the priority of each task to the corresponding value in the list *)
  (* The first task in the list will have the priority of the first element in the list *)
  (* The second task in the list will have the priority of the second element in the list *)
  (* and so on *)
let set_prio tasks prios = (
  List.iter2 (fun t p -> t.prio := p) tasks prios
)

(* task list ->unit *)
  (* Gives each task a random priority *)
let randomized_prio tasks = (
  let prios = List.init (List.length tasks) (fun i -> i + 1) in
  set_prio tasks (shuffle prios)
)

(* task list -> unit *)
  (* Gives each task a priority based on the rate monotonic *)
  (* NOTE : This function assumes that the task list is already ordered*)
let rate_monotonic_prio tasks = (
  let nb_tasks = List.length tasks in
  let prios = List.init nb_tasks (fun i -> nb_tasks - i + 1) in
  set_prio tasks prios
)

(* Packs the segments into tasks and returns a task list *)
let pack_segments (per_segs : (int * segment list) list) = (
  List.mapi (fun i (p, segs) -> generate_task (i + 1) p segs 
    (List.fold_left (fun acc s -> acc + s.wcet) 0 segs)) per_segs 
)