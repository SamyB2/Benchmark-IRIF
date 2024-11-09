module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

module Affinity = struct
  type t = (string * int) list [@@deriving show]
end

module Rt = struct
  type segment = { bcet : int; wcet : int } [@@deriving show]

  (* types for pp *)
  type graph = (string * (bool * string) list) list [@@deriving show]
  type segments = (string * segment) list [@@deriving show]

  type task = {
    priority : int;
    period : int;
    nhrt : bool;
    tc : int;
    segments : segment StringMap.t;
        [@printer fun fmt t -> pp_segments fmt (StringMap.bindings t)]
    graph : (bool * string) list StringMap.t;
        [@printer fun fmt t -> pp_graph fmt (StringMap.bindings t)]
  }
  [@@deriving show]

  type data_usage = {
    sname : string;
    r_data : string list;
    w_data : string list;
  }
  [@@deriving show]

  type task_data_usage = data_usage list [@@deriving show]
  type lock = Sequence | TaskFair | TaskFairRW | PhaseFairRW [@@deriving show]

  (* types for pp *)
  type tasks = (string * task) list [@@deriving show]
  type penalty = (string * int) list [@@deriving show]
  type usage = (string * task_data_usage) list [@@deriving show]

  type t = {
    cores : int;
    tasks : task StringMap.t;
        [@printer fun fmt t -> pp_tasks fmt (StringMap.bindings t)]
    data_penalty : int StringMap.t;
        [@printer fun fmt t -> pp_penalty fmt (StringMap.bindings t)]
    data_usage : task_data_usage StringMap.t;
        [@printer fun fmt t -> pp_usage fmt (StringMap.bindings t)]
    policy : lock;
  }
  [@@deriving show]

  let jobs graph =
    let rec aux acc jobs start =
      match acc with
      | [] -> List.map List.rev jobs
      | current :: acc -> (
          let segment = List.hd current in
          match StringMap.find_opt segment graph with
          | None ->
              Format.asprintf "segment %s cannot reach end" segment |> failwith
          | Some succ ->
              let acc, jobs, start =
                List.fold_left
                  (fun (acc, jobs, start) (loop, segment) ->
                    if segment = "end" then
                      (acc, current :: jobs, start)
                    else if loop then
                      let jobs = current :: jobs in
                      let acc, start =
                        if List.mem segment start then (acc, start)
                        else ([ segment ] :: acc, segment :: start)
                      in
                      (acc, jobs, start)
                    else if List.mem segment current then
                      Format.asprintf "there is a cycle in path %s"
                        (String.concat ", " (List.rev (segment :: current)))
                      |> failwith
                    else ((segment :: current) :: acc, jobs, start))
                  (acc, jobs, start) succ
              in
              aux acc jobs start)
    in
    match StringMap.find_opt "act" graph with
    | None -> Format.asprintf "missing act transition" |> failwith
    | Some start ->
        let _false, init = List.split start in
        aux (List.map (fun s -> [ s ]) init) [] init

  let check_FSM task =
    let snames, _segment = StringMap.bindings task.segments |> List.split in
    let snames = "act" :: "end" :: snames in
    StringMap.iter
      (fun s1 successors ->
        List.iter
          (fun (_loop, s2) ->
            let msg = Format.asprintf "segment %s is unknown" in
            if not (List.mem s1 snames) then failwith (msg s1)
            else if not (List.mem s2 snames) then failwith (msg s2))
          successors)
      task.graph;
    let _ = jobs task.graph in
    ()

  let check_task tname task =
    if not (task.period > 0) then
      Format.asprintf "the period of task %s is not positive" tname |> failwith;
    StringMap.iter
      (fun sname segment ->
        if not (segment.wcet > 0) then
          Format.asprintf "the wcet of segment %s is not positive" sname
          |> failwith
        else if not (segment.bcet <= segment.wcet) then
          Format.asprintf "bcet <= wcet is not verified for segment %s" sname
          |> failwith)
      task.segments;
    check_FSM task

  let check_data rt =
    StringMap.iter
      (fun dname penalty ->
        if not (penalty > 0) then
          Format.asprintf "the penalty of data %s is not positive" dname
          |> failwith)
      rt.data_penalty;
    let dnames, _penalty = StringMap.bindings rt.data_penalty |> List.split in
    StringMap.iter
      (fun _tname task_data_usage ->
        List.iter
          (fun { sname = _sname; r_data; w_data } ->
            List.iter
              (fun dname ->
                if not (List.mem dname dnames) then
                  Format.asprintf "data %s is unknown" dname |> failwith)
              (r_data @ w_data))
          task_data_usage)
      rt.data_usage

  let check_rt rt =
    if not (rt.cores > 0) then failwith "the number of cores must be positive";
    (* if not (StringMap.cardinal rt.tasks > rt.cores) then
       failwith "there must be more tasks than cores"; *)
    check_data rt;
    StringMap.iter check_task rt.tasks

  let check_affinity affinity rt =
    List.iter
      (fun (tname, affinity) ->
        if not (affinity >= 0 || affinity < rt.cores) then
          Format.asprintf "wrong affinity for task %s" tname |> failwith)
      affinity;
    let atname, _affinity = List.split affinity in
    let rtname, _tasks = StringMap.bindings rt.tasks |> List.split in
    let atasks, rtasks = (StringSet.of_list atname, StringSet.of_list rtname) in
    match StringSet.choose_opt (StringSet.diff rtasks atasks) with
    | Some task ->
        let msg = Format.asprintf "task %s has no affinity" task in
        failwith msg
    | None -> (
        match StringSet.choose_opt (StringSet.diff atasks rtasks) with
        | Some task ->
            let msg = Format.asprintf "task %s is not defined" task in
            failwith msg
        | None -> ())

  let check affinity rt =
    check_rt rt;
    check_affinity affinity rt
end

module Xta = struct
  type state = { name : string; invariant : string option }

  let state_to_string (state : state) =
    state.name
    ^ match state.invariant with None -> "" | Some inv -> "{ " ^ inv ^ " }"

  type transition = {
    src : string;
    dst : string;
    guard : string list;
    sync : string option;
    update : string list;
  }

  let transition_to_int trans =
    let guard =
      match trans.guard with
      | [] -> ""
      | l -> "guard " ^ String.concat "&&" l ^ "; "
    in
    let sync =
      match trans.sync with None -> "" | Some sync -> "sync " ^ sync ^ "; "
    in
    let update =
      match trans.guard with
      | [] -> ""
      | l -> "assign " ^ String.concat ",\n" l ^ "; "
    in
    trans.src ^ " -> " ^ trans.dst ^ "{ " ^ guard ^ sync ^ update ^ "}"

  type process = {
    name : string;
    param : string list;
    clock : string list;
    variable : string list;
    state : state list;
    commit : string list;
    init : string;
    trans : transition list;
  }

  let process_to_string process =
    let clock =
      match process.clock with
      | [] -> ""
      | l -> "clock " ^ String.concat ", " l ^ ";\n"
    in
    let variable = String.concat "\n" process.variable ^ "\n" in
    let state =
      match process.state with
      | [] -> ""
      | l ->
          "state\n    "
          ^ (List.map state_to_string l |> String.concat ",\n    ")
          ^ ";\n"
    in
    let commit =
      match process.commit with
      | [] -> ""
      | l -> "commit\n    " ^ String.concat ",\n    " l ^ ";\n"
    in
    let init = "init\n    " ^ process.init ^ ";\n" in
    let transition =
      match process.trans with
      | [] -> ""
      | l ->
          "trans\n    "
          ^ (List.map transition_to_int l |> String.concat ",\n    ")
          ^ ";\n"
    in
    Format.asprintf "process %s(%s){\n%s\n}" process.name
      (String.concat ", " process.param)
      (clock ^ variable ^ state ^ commit ^ init ^ transition)
end
