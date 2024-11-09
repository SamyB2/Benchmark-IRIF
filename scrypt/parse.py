
import os
import sys

filename = sys.argv[1]
output = sys.argv[2]
check_wcrt = int(sys.argv[3])

class Task :
    def __init__(self):
        self.time_exe = -1
        self.space = -1
        self.schedulable = False
        self.timeout = False
        self.task_wcrt = None
        self.nb_seg = 0
        self.nb_task = 0

    def __str__(self):
        return f"{self.time_exe} {self.schedulable} {self.task_wcrt}"

    def __set_exec_info(self, time):
        index = 0
        if len(time) > 2:
            self.timeout = True
            index = 1
        exec_info = time[index].split(",")
        self.time_exe = float(exec_info[0])
        self.space = int(exec_info[1])

    def uppaal_to_task(self, uppaal, time, info):
        self.__set_exec_info(time)
        if self.timeout : return
        self.schedulable = True if uppaal[3].split(" ")[4] == "satisfied." else False
        if not self.schedulable or check_wcrt == 0: return
        self.task_wcrt = [0] * (int)(int(info[0].split(" ")[1]) + 1)
        self.nb_task = len(self.task_wcrt) - 1  
        k = 1
        for i in range(7, len(uppaal), 4):
            self.task_wcrt[k] = int(uppaal[i].split(" ")[3])
            k +=1

    def __get_task_wcrt(self, index, hp, data, task_info):
        period = task_info[1]
        nb_seg = task_info[2]
        wcrt = 0
        for i in range(1, hp // period + 1):
            final_seg = list(map(int, data[index + nb_seg * i -1].split(",")))
            wcrt = max(wcrt, final_seg[5])
        return wcrt

    def sag_to_task(self, sag, time, info, name):
        system_info = list(map(int, info[0].split(" ")))
        hp = system_info[0]
        self.nb_task = system_info[1]
        self.nb_seg = system_info[2]
        
        self.__set_exec_info(time)
        if self.timeout : return
        self.time_exe = float(sag[6])
        self.schedulable = True if int(sag[1]) == 1 else False
        if not self.schedulable or check_wcrt == 0: 
            if os.path.exists(name + ".rta.csv"):
                os.remove(name + ".rta.csv")
            return
        with open(name + ".rta.csv") as f :
            data = f.read().split("\n")

            self.task_wcrt = [0] * (self.nb_task + 1)
            index = 1
            for i in range(1, self.nb_task + 1):
                task_info = list(map(int, info[i].split(" ")))
                self.task_wcrt[i] = self.__get_task_wcrt(index, hp, data, task_info)
                index += task_info[2] * hp // task_info[1]
        os.remove(name + ".rta.csv")

def compare (stask, utask) :
    separator = "\n" + "-"*100 + "\n"
    info_task = f"{filename} {stask.nb_task} {stask.nb_seg}\n"
    d_scheduability = "None\n"
    d_wcrt = "None\n"
    d_time = "None\n"
    d_space = "None\n"

    if utask.schedulable != stask.schedulable and not stask.timeout and not utask.timeout:
        d_scheduability = "diff schedulability\n"
        return info_task + d_scheduability + d_wcrt + d_time + d_space + separator
    
    sign_t = "=" if stask.time_exe == utask.time_exe else "<" if stask.time_exe < utask.time_exe else ">"
    d_time = f"SAG {stask.time_exe} {sign_t} UPPAAL {utask.time_exe} {stask.time_exe - utask.time_exe}\n" 
    sign_s = "=" if stask.space == utask.space else "<" if stask.space < utask.space else ">"
    d_space = f"SAG {stask.space} {sign_s} UPPAAL {utask.space} {stask.space - utask.space}\n"

    if stask.timeout or utask.timeout:
        d_scheduability = "timeout SAG UPPAAL\n" if stask.timeout and utask.timeout else "timeout SAG\n" if stask.timeout else "timeout UPPAAL\n"
        return info_task + d_scheduability + d_wcrt + d_time + d_space + separator
        
    if utask.schedulable == stask.schedulable:
        d_scheduability = "same schedulability " + ("schedulable\n" if utask.schedulable else "unschedulable\n")
        if stask.schedulable and check_wcrt == 1:
            nb_diffrent = 0
            for i in range(1, len(stask.task_wcrt)):
                dt = abs (stask.task_wcrt[i] - utask.task_wcrt[i])
                if dt > 1 : nb_diffrent += 1
            d_wcrt = f"diff in {nb_diffrent} WCRT\n"
        else :
            d_wcrt = "no WCRT\n"
    return info_task + d_scheduability + d_wcrt + d_time + d_space + separator
    
# Ouvre le fichier 'res.info' en écriture
with open(output, 'a') as f:
    info = open(filename + ".info", "r").read().split("\n")

    sag = open("tmp1", "r").read().split(",")
    timeS = open("tmp3", "r").read().split("\n")
    sag_task = Task()
    sag_task.sag_to_task(sag, timeS, info, filename)

    uppaal = open("tmp2", "r").read().split("\n")
    timeU = open("tmp4", "r").read().split("\n")
    uppaal_task = Task()
    uppaal_task.uppaal_to_task(uppaal, timeU, info)
    comparaison = compare(sag_task, uppaal_task)
    f.write(comparaison)
    os.remove(filename + ".info")
    os.remove("tmp1")
    os.remove("tmp2")
    os.remove("tmp3")
    os.remove("tmp4")
    f.close()