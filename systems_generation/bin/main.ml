open Generator.Waters
open Generator.Waters1
open Generator.Waters2
open Generator.Waters_sag
open Generator.Rnd_sys1
open Generator.Rnd_sys2
open Generator.Sys_generator
open Generator.Tsk_generator
open Generator.Seg_generator
open Generator.Scheduability_test
open Translator.Write_out

let _ = 
  let argc = Array.length Sys.argv in
  if argc < 8 then
      Printf.printf "Usage: ./generate <nb_sys> <type> <priority> <factor> <distribution> <utilization> <test> \n" 
  else 
    let nb_system = int_of_string Sys.argv.(1) 
    in
    let generate_system = match Sys.argv.(2) with
      | "waters" -> generate_waters Generator.Waters.gen_periods1
      | "waters_bis" -> generate_waters Generator.Waters.gen_periods2
      | "waters1" -> generate_waters1
      | "waters2" -> generate_waters2
      | "waters_sag" -> generate_waters_sag
      | "rnd_sys1" -> generate_rnd_sys1
      | "rnd_sys2" -> generate_rnd_sys2 Generator.Rnd_sys2.gen_periods
      | _ -> failwith "Unknown type of system. Please check README\n"
    in
    let prioritize = match Sys.argv.(3) with
      | "random" -> randomized_prio
      | "rate_monotonic" -> rate_monotonic_prio
      | _ -> failwith "Unknown type of priority. Please check README\n"
    in
    let gen_wfmax = match Sys.argv.(4) with
      | "random" -> random_wfmax
      | "fixed" -> fixed_wfmax
      | _ -> failwith "Unknown type of factors. Please check README\n"
    in
    let edit_shares = match Sys.argv.(5) with
      | "random" -> random_shares
      | "fixed" -> fixed_shares
      | _ -> failwith "Unknown type of shares. Please check README\n"
    in
    let utilization = float_of_string Sys.argv.(6)
    in
    let scheduability_test = match Sys.argv.(7) with
      | "none" -> (fun _ -> false)
      | "test1" -> test1
      | _ -> failwith "Unknown type of scheduability test. Please check README\n"
    in
    if nb_system < 1 || utilization <= 0. then
      failwith "The given arguments must be greater than 0\n"
    else
      (* generate the list of systems to verify *)
      let system_list = generate_system_list nb_system 
        (generate_system prioritize gen_wfmax edit_shares) utilization
    in
      make_out system_list scheduability_test
