open Ast.System_utils

let rec gen_WCRT_queries acc tasks = (
  match tasks with
  | [] -> acc
  | task::tl -> gen_WCRT_queries (acc ^ 
                          (Printf.sprintf "sup{%s.end} : %s.x\n" task.task_name task.task_name)) tl
)

let rec gen_scheduability_queries acc tasks = (
  match tasks with
  | [] -> acc
  | task::[] -> "A[] (" ^ acc ^ (Printf.sprintf " (not %s.error)" task.task_name)^")\n"
  | task::tl -> gen_scheduability_queries (acc ^ 
                          (Printf.sprintf "(not %s.error) and " task.task_name)) tl
)