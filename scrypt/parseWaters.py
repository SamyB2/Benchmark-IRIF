import argparse
import xml.etree.ElementTree as ET

def get_freqeuency(frequency):
    if frequency == "300MHz":
        print("Using 300MHz frequency")
        return "3.0E8"
    elif frequency == "400MHz":
        print("Using 400MHz frequency")
        return "4.0E8"
    else:
        print("Wrong frequency")
        quit()

def get_output(tsk_name, core1, core2):
    if tsk_name in core1 : return 0
    elif tsk_name in core2 : return 1
    else :  return -1

def main(frequency):
    mytree = ET.parse('ChallengeModel_w_LabelStats_fixedLabelMapping_App4mc_v072.amxmi')
    myroot = mytree.getroot()
    core1 = ["Angle_Sync", "Task_1ms"]
    core2 = ["Task_200ms", "Task_20ms", "Task_50ms",
                "Task_5ms", "Task_2ms", "Task_100ms", "Task_1000ms"]
    
    outputs = [open("./../systems_generation/system_files/waters_core1.sy", "w"), 
               open("./../systems_generation/system_files/waters_core2.sy", "w")]
    
    outputs[0].write("system waters_core1 /* 2 tasks , 189 segments */\n\n")
    outputs[1].write("system waters_core2 /* 7 tasks , 696 segments */\n\n")
    
    # By default values are in ns
    FREQUENCY = float(get_freqeuency(frequency))
    PLL = (1.0E9 / FREQUENCY)
    task_id = 0


    for task in myroot.iter('tasks'):
        tsk_name = task.get('name')
        output = get_output(tsk_name, core1, core2)
        if output == -1 : continue
        tsk_priority = task.get('priority')
        tsk_stimuli = task.get('stimuli').split('?')
        if (tsk_stimuli[1] == "type=Periodic"):
            if (tsk_stimuli[0].split('_')[1][-2:] == "us"):
                tsk_period = int(float(tsk_stimuli[0].split('_')[1][:-2]) * 1000)
            elif (tsk_stimuli[0].split('_')[1][-2:] == "ms"):
                tsk_period = int(float(tsk_stimuli[0].split('_')[1][:-2]) * 1000000)
            else:
                print("Wrong unit used for the period")
                quit()
        else:
            if (tsk_stimuli[0].split('_')[1][-2:] == "us"):
                tsk_period = int(float(tsk_stimuli[0].split('_')[1][:-2]) * 1000)
            elif (tsk_stimuli[0].split('_')[1][-2:] == "ms"):
                tsk_period = int(float(tsk_stimuli[0].split('_')[1][:-2]) * 1000000)
            else:
                print("Wrong unit used for the period")
                quit()
        task_id += 1
        task_sum_wcet = 0
        task_st = ""

        run_st = ""
        run_id = 1
        for runnable in task.iter('calls'):
            run_name = runnable.get('runnable').split('?')[0]
            
            st = ".//*[@name='"+run_name+"']"
            runnable = myroot.findall(st)[0]
            bcet = float(runnable.findall(".//*/lowerBound")[0].get('value'))
            bcet = int(bcet * PLL)
            wcet = float(runnable.findall(".//*/upperBound")[0].get('value'))
            wcet = int(wcet * PLL)
            task_sum_wcet += wcet
            run_st += f"\tsegment {run_name} {run_id} {bcet} {wcet}\n"
            run_id += 1
        print(">>>>>>>>" , tsk_name)
        task_st += f"task {tsk_name} {task_id} {tsk_period} {tsk_priority} {task_sum_wcet} /* period => {tsk_period/1000000} ms , C => {task_sum_wcet/1000000} ms */\n"
        task_st += run_st + "\n"
        outputs[output].write(task_st)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--frequency", type=str, help="Frequency to use (300MHz | 400MHz)", default = "400MHz")
    args = parser.parse_args()
    main(args.frequency)