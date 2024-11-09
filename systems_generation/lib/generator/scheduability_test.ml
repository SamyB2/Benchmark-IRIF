open Ast.System_utils

let max_seg task = 
  List.fold_left (fun acc (seg:segment) -> 
    if seg.wcet > acc then seg.wcet else acc
  ) 0 !(task.segments)

let blocking lp = 
  List.fold_left (fun acc t -> 
    let max_seg_t = max_seg t in
    if max_seg_t > acc then max_seg_t else acc
  ) 0 lp
  

let final task = 
  let seg_list = !(task.segments) in
  List.fold_left (fun _ s -> s.wcet) 0 seg_list
  

let test1 tasks = 
  List.for_all (
    fun task -> (
      let lp = List.filter (fun t -> !(task.prio) > !(t.prio)) tasks in 
      let hp = List.filter (fun t -> !(task.prio) < !(t.prio)) tasks in
      let blocking_t = blocking lp in
      let pt = float_of_int task.period in
      let final_t = float_of_int (final task) in
      let wj = !(task.sum_wcet) in
        (blocking_t + wj + 
        (int_of_float 
          (List.fold_left (fun acc t -> 
            let w_t = float_of_int !(t.sum_wcet) in
            let u_t = (float_of_int !(t.sum_wcet)) /. (float_of_int t.period) in
            acc +. w_t +. u_t *. (pt -. final_t -. w_t)) 0.0 hp))) <= task.period
    )
  ) tasks
