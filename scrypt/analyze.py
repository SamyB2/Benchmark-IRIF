import os

def analyze_test(data) :
    nb_sched = 0
    for line in data :
        if "is schedulable" in line :
            nb_sched += 1
    print(f"\tWith the scheduability test used\n")
    print(f"\t{nb_sched} systems are schedulable\n")
    print(f"\t{len(data) - nb_sched -1} systems are potentially not schedulable\n")
    print(f"\tWhich gives us a scheduability ratio of {nb_sched/(len(data) - 1) * 100}%\n")

def analyze_result(data, utilization) :
    nb_system = len(data)-1
    nb_sched = 0
    nb_non_sched = 0
    diff_scheduability = 0
    diff_wcrt = 0
    sum_t_sag = 0
    sum_t_upp = 0
    sum_dt = 0
    sum_ds = 0
    sum_tasks = 0
    sums_segs = 0
    pt_sag = 0
    pt_upp = 0
    pt_eq = 0
    ps_sag = 0
    ps_upp = 0
    ps_eq = 0
    p_out_sag = 0
    p_out_upp = 0

    for i in range(nb_system) :
        system = data[i].split("\n")
        info_sys = system[0].split(" ")
        info_scheduability = system[1]
        sum_tasks += int(info_sys[1])
        sums_segs += int(info_sys[2])


        if info_scheduability == "diff schedulability" :
            diff_scheduability += 1
            continue
        # get the time information
        info_time = system[3].split(" ")
        sum_t_sag += float(info_time[1])
        sum_t_upp += float(info_time[4])
        if info_time[2] == "<" :
            pt_sag += 1
        elif info_time[2] == ">" :
            pt_upp += 1
        else :
            pt_eq += 1
        sum_dt += float(info_time[5])

        # get the space information
        info_space = system[4].split(" ")
        if info_space[2] == "<" :
            ps_sag += 1
        elif info_space[2] == ">" :
            ps_upp += 1
        else :
            ps_eq += 1
        sum_ds += int(info_space[5])

        # check the timeout
        if info_scheduability == "timeout SAG UPPAAL" : 
            p_out_sag += 1
            p_out_upp += 1
            continue  
        if info_scheduability == "timeout SAG" :
            p_out_sag += 1
            continue
        if info_scheduability == "timeout UPPAAL" : 
            p_out_upp += 1
            continue
        
        info_scheduability = info_scheduability.split(" ")
        if info_scheduability[0] == "same" :
            if info_scheduability[2] == "schedulable" :
                nb_sched += 1
            else :
                nb_non_sched += 1
            info_wcrt = system[2].split(" ")
            diff_wcrt += int(info_wcrt[2]) if len(info_wcrt) > 2 else 0
            continue
        else : 
            print("Error in the parsing of the data")
            exit(1)


    print(f"On {nb_system} systems with a utilization of {utilization}:\n")
    print(f"With {nb_sched} schedulable and {nb_non_sched} non-schedulable systems\n")
    print(f"With a scheduability ration of {nb_sched/nb_system * 100}%\n")
    if os.path.exists("../systems_generation/system_files/systems.t") :
        data_test = open("../systems_generation/system_files/systems.t", "r").read().split("\n")
        analyze_test(data_test)
    print(f"Average number of tasks per system : {sum_tasks/nb_system}\n")
    print(f"Average number of segments per task: {sums_segs/nb_system}\n")
    print(f"Number of different schedulability: {diff_scheduability}\n")
    print(f"Number of different WCRT: {diff_wcrt}\n")
    print(f"UPPAAL performed better than SAG in {pt_upp} cases\n")
    print(f"SAG performed better than UPPAAL in {pt_sag} cases\n")
    print(f"UPPAAL and SAG performed equally in {pt_eq} cases\n")
    print(f"Average UPPAAL's execution time: {sum_t_upp/nb_system} seconds\n")
    print(f"Average SAG's execution time: {sum_t_sag/nb_system} seconds\n")
    if sum_t_sag > sum_t_upp :
        print(f"On average, UPPAAL is {sum_t_sag/sum_t_upp} times faster than SAG\n")
    else :
        print(f"On average, SAG is {sum_t_upp/sum_t_sag} times faster than UPPAAL\n")
    if p_out_sag != 0 : 
        print(f"SAG timed-out in {p_out_sag} cases\n")
    if p_out_upp != 0 : 
        print(f"UPPAAL timed-out in {p_out_upp} cases\n")

    print("In terms of time:\n")
    if pt_sag > pt_upp :
        print(f"\tSAG is better than UPPAAL in {pt_sag/(nb_system - pt_eq) * 100}% of the cases\n")
    elif pt_sag < pt_upp :
        print(f"\tUPPAAL is better than SAG in {pt_upp/(nb_system - pt_eq) * 100}% of the cases\n")
    else :
        print(f"\tUPPAAL and SAG are equally good in {pt_eq/(nb_system) * 100}% of the cases\n")
    avg = sum_dt/nb_system
    if avg < 0 : 
        print(f"\t In average, SAG is {-avg} seconds faster than UPPAAL\n")
    else :
        print(f"\t In average, UPPAAL is {avg} seconds faster than SAG\n")

    print("In terms of space:\n")
    if ps_sag > ps_upp :
        print(f"\tSAG is better than UPPAAL in {ps_sag/(nb_system - ps_eq) * 100}% of the cases\n")
    elif ps_sag < ps_upp :
        print(f"\tUPPAAL is better than SAG in {ps_upp/(nb_system - ps_eq) * 100}% of the cases\n")
    else :
        print(f"\tUPPAAL and SAG are equally good in {ps_eq/(nb_system) * 100}% of the cases\n")
    avg = sum_ds/nb_system
    if avg < 0 : 
        print(f"\t In average, SAG uses {-avg} bytes less than UPPAAL\n")
    else :
        print(f"\t In average, UPPAAL uses {avg} bytes less than SAG\n")

utilizations = [".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9"]
separator = "\n" + "-"*100 + "\n"
for u in utilizations :
    data = open(f"results_{u}.info", "r").read().split(separator)
    analyze_result(data, u)
    print(separator)

