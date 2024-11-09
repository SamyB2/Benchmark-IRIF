open Ast

let default_variable nb_task =
  Format.asprintf "bool busy[ht] := { %s };\ntab_core tab[ht] := { %s };\n "
    (String.concat ", " (List.init (nb_task + 1) (fun _i -> "false")))
    (String.concat ", " (List.init (nb_task + 1) (fun _i -> "{-1,-1}")))

let default_channel =
  "chan insert, ter, concede;\n\
   broadcast chan procede, exe, toend;\n\
   chan urg[ht];\n\
   chan priority exe < concede;\n\
   chan priority procede < insert;\n\
   chan priority exe < insert;\n\
   chan priority urg < insert;\n"

let declaration =
  Format.asprintf
    "/* highest number of tasks assigned to a given core + 1*/\n\
     const int ht := %d;\n\n\
     /* type for scheduler array cell */\n\
     typedef struct {int id; int pr;} tab_core;\n\n\
     /* constants : tasks IDs */\n\
     %s\n\
     /* constants : task priorities */\n\
     %s\n\
     /* variables */\n\
     %s\n\
     /* channels */\n\
     %s\n"

let process =
  Format.asprintf
    "\n\n\
     process task_%s(const int id, const int pr) {\n\
    \    clock x,y;\n\
    \    const int period := %d;\n\
     %s\n\
     state\n\
    \    start,\n\
    \    wait { x <= period },\n\
    \    act,\n\
     %s\n\
    \    end,\n\
    \    error;\n\
     commit\n\
    \    start,\n\
    \    error,\n\
    \    end;\n\
     init\n\
    \    start;\n\
     trans\n\
    \    start -> act { sync insert!; assign add_to_array (tab, id, pr); },\n\
    \    wait -> act { guard x == period; sync insert!; assign \
     x:=0,add_to_array (tab, id, pr); },\n\
     %s\n\
    \    end -> wait { guard x <= period; sync ter!; };\n\
     }"

let instantiation tnames =
  let task_instantiation tname =
    Format.asprintf "%s := task_%s (%s_id, %s_pr);" tname tname tname tname
  in
  Format.asprintf
    "/* process instantiation */\n\
     scheduler := Scheduler();\n\
     %s\n\n\
     /* composition */\n\
     system %s"
    (List.map task_instantiation tnames |> String.concat "\n")
    (String.concat ", " tnames ^ ", scheduler;")

type core = (string * Rt.task) list [@@deriving show]

let declaration core =
  let nb_tasks = List.length core in
  declaration (nb_tasks + 1)
    (List.mapi
       (fun i (tname, _task) ->
         Format.asprintf "const int %s_id := %d;\n" tname i)
       core
    |> String.concat "")
    (List.map
       (fun (tname, task) ->
         Format.asprintf "const int %s_pr := %d;\n" tname task.Rt.priority)
       core
    |> String.concat "")
    (default_variable nb_tasks)
    default_channel

type data = (string * int) list [@@deriving show]

module IntSet = Set.Make (Int)

let get_data rt =
  let accumulator tname acc Rt.{ sname; r_data; w_data } =
    StringMap.mapi
      (fun key value ->
        let dataIn, dataOut = value in
        let dataIn =
          if List.mem key r_data then (sname, tname) :: dataIn else dataIn
        in
        let dataOut =
          if List.mem key w_data then (sname, tname) :: dataOut else dataOut
        in
        (dataIn, dataOut))
      acc
  in
  let init =
    StringMap.fold
      (fun data _penalty acc -> StringMap.add data ([], []) acc)
      rt.Rt.data_penalty StringMap.empty
  in
  StringMap.fold
    (fun tname data_usage acc ->
      List.fold_left (accumulator tname) acc data_usage)
    rt.data_usage init

let get_loop task =
  StringMap.fold
    (fun _s1 successors acc ->
      List.fold_left
        (fun acc (loop, s2) -> if loop then StringSet.add s2 acc else acc)
        acc successors)
    task.Rt.graph StringSet.empty

let filtered_task task =
  let jobs = Rt.jobs task.Rt.graph in
  let reachable = "act" :: List.concat jobs in
  let segments =
    StringMap.filter
      (fun sname _segment -> List.mem sname reachable)
      task.segments
  in
  let graph =
    StringMap.filter
      (fun sname _successors -> List.mem sname reachable)
      task.graph
  in
  { task with segments; graph }

let policy lock single nb_cores penalty write =
  if lock = Rt.TaskFair then (nb_cores - 1) * penalty
  else if single then if write then penalty else 2 * penalty
  else if lock = TaskFairRW then (nb_cores - 1) * penalty
  else if write then 2 * (nb_cores - 1) * penalty
  else 2 * penalty

let is_single_writer rt =
  get_data rt
  |> StringMap.for_all (fun _dname (_segmentInt, segmentOut) ->
         List.split segmentOut |> snd |> StringSet.of_list |> StringSet.cardinal
         < 2)

(* TODO *)
let get_overhead affinity rt =
  (* let nb_cores (_sname, tname) segments =
       let rec aux segments res =
         match segments with
         | [] -> IntSet.cardinal (IntSet.remove (List.assoc tname affinity) res)
         | s::segments -> aux segments (IntSet.add (List.assoc s affinity) res)
       in
       aux segments IntSet.empty
     in *)
  let has_overhead (_sname, tname) segments =
    List.exists
      (fun (_sname, tname2) ->
        List.assoc tname affinity <> List.assoc tname2 affinity)
      segments
  in
  let accumulator dname (segmentIn, segmentOut) acc =
    let is_single_writer = is_single_writer rt in
    let policy =
      StringMap.find dname rt.data_penalty
      |> policy rt.Rt.policy is_single_writer rt.cores
    in
    let overhead acc d write =
      List.fold_left
        (fun acc (sname, tname) ->
          if has_overhead (sname, tname) segmentOut then
            StringMap.update sname
              (fun value ->
                let toAdd = policy write in
                match value with
                | Some v -> Some (v + toAdd)
                | None -> Some toAdd)
              acc
          else acc)
        acc d
    in
    let acc = overhead acc segmentIn false in
    overhead acc segmentOut true
  in
  StringMap.fold accumulator (get_data rt) StringMap.empty

type xta = Rt.task list

module IntMap = Map.Make (Int)

let get_cores affinity rt =
  (* let overhead = get_overhead affinity rt in *)
  (* let add_overhead sname segment =
       match StringMap.find_opt sname overhead with
       | None -> segment
       | Some o ->
           let wcet = segment.Rt.wcet + o in
           { segment with wcet }
     in *)
  StringMap.fold
    (fun tname task acc ->
      (* let segments = StringMap.mapi add_overhead task.Rt.segments in *)
      (* let task = { task with segments } in *)
      IntMap.update
        (List.assoc tname affinity)
        (function
          | None -> Some [ (tname, task) ]
          | Some tasks -> Some ((tname, task) :: tasks))
        acc)
    rt.Rt.tasks IntMap.empty

let variable task =
  let tolerance =
    if task.Rt.tc > 2 then
      [
        Format.asprintf "    const int error_tolerance := %d;\nint na := 0;"
          task.tc;
      ]
    else []
  in
  get_loop task
  |> StringSet.map (fun s -> Format.asprintf "    bool loop_%s := false;" s)
  |> StringSet.elements |> ( @ ) tolerance |> String.concat "\n"

let state task highest =
  let state_format = Format.asprintf "    %s { %s }," in
  let state_no_inv = Format.asprintf "    %s," in
  let overshoot =
    if task.Rt.tc > 1 then
      [
        state_format "overshoot"
          (Format.asprintf "x <= %s * period"
             (if task.tc == 2 then "2" else "na"));
      ]
    else []
  in
  StringMap.fold
    (fun sname segment acc ->
      let acc =
        state_format sname (Format.asprintf "y <= %d" segment.Rt.wcet) :: acc
      in
      if
        (not highest)
        && StringMap.find sname task.graph
           |> List.exists (fun (loop, s) -> (not loop) && s <> "end")
      then state_no_inv (sname ^ "_concede") :: acc
      else acc)
    task.Rt.segments overshoot
  |> List.rev |> String.concat "\n"

let transitions task highest =
  let trans_format =
    Format.asprintf "    %s -> %s { guard %s; sync %s; assign %s; },"
  in
  let trans_no_assign =
    Format.asprintf "    %s -> %s { guard %s; sync %s; },"
  in
  let trans_no_guard =
    Format.asprintf "    %s -> %s { sync %s; assign %s; },"
  in

  let error =
    [
      trans_no_assign "end" "error"
        (Format.asprintf "x > %speriod"
           (if task.Rt.tc > 2 then "error_tolerance*"
            else if task.tc == 2 then "2*"
            else ""))
        "ter!";
    ]
  in
  let end_to_overshoot =
    if task.tc > 2 then
      trans_format "end" "overshoot" "x > period && x <= 2*period" "ter!"
        "na := 2"
      :: List.init (task.tc - 2) (fun na ->
             trans_format "end" "overshoot"
               (Format.asprintf "x > %d*period && x <= %d*period" (na + 2)
                  (na + 3))
               "ter!"
               (Format.asprintf "na := %d" (na + 3)))
    else if task.tc > 1 then
      [ trans_no_assign "end" "overshoot" "x > period && x <= 2*period" "ter!" ]
    else []
  in
  let overshoot_to_act =
    if task.tc > 1 then
      [
        trans_format "overshoot" "act"
          (Format.asprintf "x == %s*period" (if task.tc = 2 then "2" else "na"))
          "insert!" "x:=0; add_to_array(tab, id,pr)";
      ]
    else []
  in

  let loop = get_loop task |> StringSet.elements in

  let act_to_loop =
    List.map
      (fun s ->
        trans_format "act" s s "urg[id]?" ("loop_" ^ s ^ ":=false, y:=0"))
      loop
  in
  let rec get_reachable_segments sname graph acc =
    let succs = StringMap.find sname graph in
    List.fold_left
      (fun acc (_, succ) ->
        if succ = "end" then acc
        else
        if List.mem succ acc then acc
        else get_reachable_segments succ graph (succ :: acc)) acc succs
  in
  let sum_wcet snames segments =
    List.fold_left
      (fun acc sname -> acc + (StringMap.find sname segments).Rt.wcet)
      0 snames
  in

  let segment_to_concede =
    if highest then []
    else
      List.filter_map
        (fun (sname, segment) ->
          match
            List.exists
              (fun (loop, succ) -> (not loop) && succ <> "end")
              (StringMap.find sname task.graph)
          with
          | false -> None
          | true ->
              Some
                (trans_no_assign sname (sname ^ "_concede")
                   (Format.asprintf "y>=%d" segment.Rt.bcet)
                   "concede!"))
        (StringMap.bindings task.segments)
  in
  let act_to_error = 
    [
      trans_no_assign "act" "error" (
        Format.asprintf "x> period-%d" 
          (sum_wcet (get_reachable_segments "act" task.graph []) task.segments)
      ) "urg[id]?"
    ]
  in
  let translate sname succs acc =
    if sname = "act" then
      List.map
        (fun (_false, succ) ->
          let trans =
            match loop with
            | [] -> trans_no_guard
            | _ ->
                fun x y ->
                  trans_format x y ("!(" ^ String.concat " || " loop ^ ")")
          in
          trans "act" succ "urg[id]?" "y:= 0")
        succs
      @ acc
    else
      let guard =
        Format.asprintf "y>=%d" (StringMap.find sname task.segments).bcet
      in
      acc
      @ List.map
          (fun (loop, succ) ->
            if loop then
              let sync = if highest then "exe!" else "toend!" in
              trans_format sname "end" guard sync ("loop_" ^ succ ^ " := true")
            else if succ = "end" then trans_no_assign sname "end" guard "toend!"
            else
              trans_format sname succ guard "exe!" "y:=0"
              ^
              if highest then ""
              else (trans_no_guard (sname ^ "_concede") succ "urg[id]?" "y:=0") ^ 
                    (trans_no_assign (sname ^ "_concede") "error" (
                      Format.asprintf "x> period-%d" (sum_wcet (get_reachable_segments sname task.graph []) task.segments)
                    ) "urg[id]?"
                    )
          )
          succs
  in
  act_to_loop @ segment_to_concede
  @ StringMap.fold translate task.graph []
  @ end_to_overshoot @ overshoot_to_act @ error @ act_to_error 
  |> String.concat "\n"

let task_to_xta (tname, task) highest =
  let task = filtered_task task in
  process tname task.Rt.period (variable task) (state task highest)
    (transitions task highest)

let core_to_xta functions_scheduler core =
  let highest =
    List.map (fun (_tname, task) -> task.Rt.priority) core
    |> List.fold_left (fun acc pr -> if acc > pr then acc else pr) 0
  in
  let tasks =
    List.map
      (fun (tname, task) -> task_to_xta (tname, task) (task.priority = highest))
      core
    |> String.concat ""
  in
  let instantiation = instantiation (List.split core |> fst) in
  declaration core ^ functions_scheduler ^ tasks ^ "\n\n" ^ instantiation

let rt_to_xta affinity rt =
  let cores = get_cores affinity rt in
  let functions_scheduler =
    let ic = open_in "xta.txt" in
    let rec loop res =
      try
        let line = input_line ic in
        loop (res ^ "\n" ^ line)
      with End_of_file -> res
    in
    loop ""
  in
  IntMap.map (core_to_xta functions_scheduler) cores
