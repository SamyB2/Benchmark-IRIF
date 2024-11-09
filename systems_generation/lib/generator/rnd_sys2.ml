open Ast.System_utils
open Tsk_generator

(* The periods are : 1, 2, 3, 5, 6, 7, 10, 11, 15 in Milisecnds *)
  (* The unit used is Nanoseconds *)
let periods = [
  1000000; 2000000; 3000000;
  5000000; 6000000; 7000000;
  10000000; 11000000; 15000000;
]

(* unit -> int list *)
  (* You can edit this function or the list above to generate systems with *)
  (* Your own periods *)
let gen_periods () = (
  let nb_tasks = 2 + Random.int 6 in
  get_first_n (shuffle periods) [] nb_tasks
)

(* float -> int -> int list -> float * int * int list *)
  (* Given a utilization, a fixed number of segment and a list of periods *)
  (* This function distributes randomly the utilization and the number of segment *)
  (* On all the periods *)
let distribute_utilization_segments utilization segments periods = (
  let rec distribute_utilization_aux utilization segments periods acc = (
    match periods with
      | [] -> acc
      | p :: [] -> distribute_utilization_aux 0. 0 [] ((p, utilization, segments)::acc)
      | p :: tl -> (
        let u = 0.0001 +. Random.float utilization in
        let nb_seg = 1 + Random.int segments in
        let du = (
          if utilization -. u <= 0. then utilization
          else utilization -. u
        )
        in
        let ds = (
          if segments - nb_seg <= 0 then 1
          else segments - nb_seg
        )
        in
        distribute_utilization_aux du ds tl ((p, u, nb_seg)::acc)
      )
  )
  in
  distribute_utilization_aux utilization segments periods []
)

(* float -> int *)
(* This function gives a number of segments depending on the given utilization *)
let get_nb_segments utilization = (
  match utilization with 
  | _ when utilization < 0.1 -> 100 + Random.int 100
  | _ -> int_of_float (utilization *. 1000.) + Random.int 100
)

(* int -> int -> segment list *)
  (* This function generates a list of segments with a given execution time *)
  (* And a given number of segments *)
let generate_segments exec_time nb_segs = (
  let rec generate_segments_aux exec_time nb_segs acc = (
    match nb_segs with
      | 0 -> acc
      | 1 -> (
        let segment_name = "seg_" ^ (string_of_int nb_segs) in
        let wcet = exec_time in
        let bcet = 1 + Random.int wcet in
        let seg = {segment_name; id = nb_segs; bcet; wcet} in
        generate_segments_aux 0 0 (seg::acc)
      )
      | id -> (
        let avg = exec_time / nb_segs in
        let segment_name = "seg_" ^ (string_of_int id) in
        let wcet = 2 + Random.int avg in
        let bcet = 1 + Random.int wcet in
        let seg = {segment_name; id; bcet; wcet} in
        let d_wcet = (
          if exec_time - wcet <= 0 then exec_time
          else exec_time - wcet
        ) in 
        generate_segments_aux d_wcet (nb_segs - 1) (seg::acc)
      )
  )
  in generate_segments_aux exec_time nb_segs []
)

let generate_tasks per_u_seg = (
  let rec generate_tasks_aux per_u_seg acc = (
    match per_u_seg with
      | [] -> List.rev acc
      | (p, u, nb_segs) :: tl -> (
        let id = 1 + List.length acc  in
        let task_name = "task_" ^ (string_of_int id) in
        let period = p in
        let exec_time = int_of_float (float_of_int p *. u) in
        let segments = generate_segments exec_time nb_segs in
        let sum_wcet = ref exec_time in
        let prio = ref 0 in
        let task = {task_name; id; period; segments = ref segments; sum_wcet; prio} in
        generate_tasks_aux tl (task::acc)
      )
  )
  in generate_tasks_aux per_u_seg [] 
)

(* Note That: By providing another function for the period's generation *)
  (* You can use this function to generate systems with your own periods*)
let generate_rnd_sys2 gen_periods prioritize _ _ name utilization= (
  let periods = gen_periods () in
  let nb_segs = get_nb_segments utilization in
  let per_u_seg = distribute_utilization_segments utilization nb_segs periods in
  let tasks = generate_tasks per_u_seg in
  prioritize tasks;
  Some {system_name = name; tasks; hyperperiod = ref 0L}
)