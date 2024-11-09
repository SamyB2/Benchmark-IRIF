let generate_system_list nb generate_system utilization  = ( 
  Random.self_init ();
  let rec aux acc nb = 
    if nb = 0 then acc
    else
      let system = generate_system ("system_" ^ (string_of_int nb)) utilization in
      match system with
      | None -> aux acc nb
      | Some s -> aux (s::acc) (nb - 1)
  in aux [] nb
)
