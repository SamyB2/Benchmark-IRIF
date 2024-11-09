type segment = {
  segment_name : string;
  id : int;
  bcet : int;
  wcet : int;
}

type task = {
  task_name : string;
  id : int; 
  prio : int ref; 
  period : int;
  sum_wcet : int ref;
  segments : segment list ref
}

type system = {
  system_name : string;
  tasks : task list;
  hyperperiod : int64 ref;
}