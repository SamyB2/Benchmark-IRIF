%{
    open Ast.System_utils
%}

(* token for eof *)
%token EOF

(* tokens for key words *)
%token SYSTEM, TASK, SEGMENT

(* token for values *)
%token <string> NAME
%token <int> NUMBER

%start <Ast.System_utils.system> system

%%

system:
    | SYSTEM name=NAME tasks=task+ EOF {
        let sys_name = name in 
        let sys_tasks = tasks in
        let hp = ref 0L in 
        {system_name = sys_name; tasks = sys_tasks; 
        hyperperiod = hp}
    }

task: 
    | TASK name=NAME id=NUMBER period=NUMBER prio=NUMBER wcet=NUMBER segments=segment+ {
        {task_name = name; id = id; period = period; 
        prio = ref prio; sum_wcet = ref wcet; segments = ref segments}
    }

segment:
    | SEGMENT name=NAME id=NUMBER bcet=NUMBER wcet=NUMBER {
        let seg_name = name in 
        let seg_id = id in 
        let seg_bcet = bcet in 
        let seg_wcet = wcet in 
        {segment_name = seg_name; id = seg_id;
         bcet = seg_bcet; wcet = seg_wcet}
    }