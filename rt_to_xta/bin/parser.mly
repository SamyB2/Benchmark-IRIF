%{
  open Ast
  open Ast.Rt

  let list_to_stringMap l =
    List.fold_left
      (fun acc (key, value) -> StringMap.add key value acc)
      StringMap.empty l
%}

%token CORE DATA IN OUT TASK SEGMENT TRANSITION ARROW ACT END LOOP
%token POLICY SEQUENCE TASKFAIR TASKFAIRRW PHASEFAIRRW EOF
%token<int> NUM
%token<string> ID AFFINITY

%start<Ast.Rt.t> rt
%start<Ast.Affinity.t> affinity

%%

affinity:
| a=affinity_item+ EOF { a }

affinity_item: task=AFFINITY core=NUM { task, core }


rt:
| CORE cores=NUM tasks=task+ data_penalty=data_decl? policy_decl=policy_decl? EOF
{
  let tasks, dataUse = List.split tasks in
  let tasks = list_to_stringMap tasks in
  let data_usage = list_to_stringMap dataUse in
  {
    cores;
    tasks;
    data_penalty = Option.value data_penalty ~default:StringMap.empty;
    data_usage;
    policy = Option.value policy_decl ~default:Sequence;
  }
}

task :
| TASK name=ID priority=NUM period=NUM tolerance=NUM? segments=segment+ TRANSITION transitions=transition+
{
  let segments, data_usage = List.split segments in
  let segments = list_to_stringMap segments in
  let graph =
    let rec aux transitions graph =
      match transitions with
      | [] -> graph
      | (s1, s2) :: transitions ->
          let graph =
            StringMap.update s1
              (function None -> Some [ s2 ] | Some s -> Some (s2 :: s))
              graph
          in
          aux transitions graph
    in
    aux transitions StringMap.empty
  in
  ( ( name,
      {
        priority;
        period;
        nhrt = Option.is_some tolerance;
        tc = Option.value tolerance ~default:1;
        segments;
        graph;
      } ),
    (name, data_usage) )
}

segment :
| SEGMENT name=ID et=et data=data_usage
{
  ( (name, { bcet = fst et; wcet = snd et }),
    { sname=name; r_data = fst data; w_data = snd data } )
}

et:
| wcet=NUM          { 0, wcet    }
| bcet=NUM wcet=NUM { bcet, wcet }

data_usage :
|                               { ([], [])          }
| IN data=ID+                   { (data, [])        }
| OUT data=ID+                  { ([], data)        }
| IN dataIn=ID+ OUT dataOut=ID+
| OUT dataOut=ID+ IN dataIn=ID+ { (dataIn, dataOut) }


transition :
| segment1=segment1 ARROW loop=LOOP? segment2=segment2 { (segment1, (Option.is_some loop, segment2)) }

segment1 :
| ACT        { "act"   }
| segment=ID { segment }

segment2 :
| END        { "end"   }
| segment=ID { segment }


data_decl : DATA data=data+ { list_to_stringMap data }

data : name=ID penalty=NUM { (name, penalty) }


policy_decl : POLICY policy=lock { policy }

lock :
| SEQUENCE    { Sequence    }
| TASKFAIR    { TaskFair    }
| TASKFAIRRW  { TaskFairRW  }
| PHASEFAIRRW { PhaseFairRW }
