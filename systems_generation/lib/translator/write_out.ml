open Ast.System_utils 
open Translation
open Queries

let write_out output str = 
  let fd = open_out output in
  Printf.fprintf fd "%s" str;
  close_out fd

let make_out systems scheduability_test =
  write_out "systems_generation/system_files/systems.t" (systems_to_t systems scheduability_test);
  List.iter (fun s -> 
    write_out ("systems_generation/system_files/" ^ s.system_name ^ ".sy") (system_to_sy s);
    write_out ("systems_generation/uppaal_files/" ^ s.system_name ^ ".rt") (system_to_rt s);
    write_out ("systems_generation/uppaal_files/" ^ s.system_name ^ ".sol") (system_to_sol s);
    write_out ("systems_generation/uppaal_files/" ^ s.system_name ^ "_0.q") (gen_scheduability_queries "" s.tasks);
    write_out ("systems_generation/uppaal_files/" ^ s.system_name ^ "_1.q") (gen_WCRT_queries "" s.tasks);
    write_out ("systems_generation/sag_files/" ^ s.system_name ^ ".csv") (system_to_csv s);
    write_out ("./scrypt/" ^ s.system_name ^ ".info") (system_to_info s);
  ) systems ;
