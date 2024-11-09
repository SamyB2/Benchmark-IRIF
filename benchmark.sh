#!/bin/bash
## IMPORTANT: make sure to build and clear the directories before running the benchmark

# function to generate xta files from .rt and .sol files
gen_xta () {
    check_wcrt=$1
    mv systems_generation/uppaal_files/system_*.rt rt_to_xta/examples/
    mv systems_generation/uppaal_files/system_*.sol rt_to_xta/examples/

    cd rt_to_xta/
    dune build
    for fic in examples/system*.rt
    do
        fic_no_ext=$(basename $fic .rt)
        ./_build/default/bin/main.exe "./examples/$fic_no_ext" > /dev/null
    done
    rm examples/system*.rt
    rm examples/system*.sol
    mv examples/system* ../systems_generation/uppaal_files/
    cd $OLDPWD
}

# function to compute the schedulability analysis using np-schedulability-analysis
compute_sag () {
    sag_file=$1
    check_wcrt=$2
    time_limit=$3
    if [ $check_wcrt -eq 1 ]; then
        /usr/bin/time -f "%e, %M" 2>| ./scrypt/tmp3 timeout "${time_limit}" ./np-schedulability-analysis/build/nptest "${sag_file}.csv" -r >| ./scrypt/tmp1
    else
        /usr/bin/time -f "%e, %M" 2>| ./scrypt/tmp3 timeout "${time_limit}" ./np-schedulability-analysis/build/nptest "${sag_file}.csv" >| ./scrypt/tmp1
    fi
    if [ ! $? -eq 124 ] && [ $check_wcrt -eq 1 ]; then
        mv "${sag_file}.rta.csv" ./scrypt/
        return
    fi
    rm "${sag_file}.csv"   
}

# function to compute the schedulability analysis using Uppaal
compute_uppaal () {
    uppaal_file=$1
    check_wcrt=$2
    time_limit=$3
    if [ $check_wcrt -eq 1 ]; then
        /usr/bin/time -f "%e, %M" 2>| ./scrypt/tmp4 timeout "${time_limit}" ./uppaal-5.1.0-beta5-linux64/bin/verifyta.sh -T -C -q -s "${uppaal_file}_0.xta" >| ./scrypt/tmp2
        if grep -q "is satisfied" ./scrypt/tmp2; then
            cat "${uppaal_file}_1.q" >> "${uppaal_file}_0.q"
            /usr/bin/time -f "%e, %M" 2>| ./scrypt/tmp4 ./uppaal-5.1.0-beta5-linux64/bin/verifyta.sh -T -C -q -s "${uppaal_file}_0.xta" >| ./scrypt/tmp2
        fi
    else
        /usr/bin/time -f "%e, %M" 2>| ./scrypt/tmp4 timeout "${time_limit}" ./uppaal-5.1.0-beta5-linux64/bin/verifyta.sh -T -C -q -s "${uppaal_file}_0.xta" >| ./scrypt/tmp2
    fi
    rm "${uppaal_file}_0.xta"
    rm "${uppaal_file}_0.q"
    rm "${uppaal_file}_1.q"
}

# function to parse the result of each computes file
parse_result () {
    name=$1
    output_file=$2
    check_wcrt=$3
    cd ./scrypt 
    python3 parse.py $name $output_file $check_wcrt
    cd ..
}

# function to evaluate a set of systems
evaluate_set () {
    set_size=$1
    output_file=$2
    check_wcrt=$3
    time_limit=$4
    for i in $(seq 1 $set_size); do
        name="system_$i"
        sag_file="systems_generation/sag_files/$name"
        uppaal_file="systems_generation/uppaal_files/$name"

        compute_sag $sag_file $check_wcrt $time_limit

        compute_uppaal $uppaal_file $check_wcrt $time_limit

        parse_result $name $output_file $check_wcrt

    done
}

evaluate_set2 () {
    set_size=$1
    nb_satisfied=0
    for i in $(seq 1 $set_size); do
        name="system_$i"
        uppaal_file="systems_generation/uppaal_files/$name"
        
        /usr/bin/time -f "%e, %M" 2>| ./scrypt/tmp4 ./uppaal-5.1.0-beta5-linux64/bin/verifyta.sh -T -C -q -s "${uppaal_file}_0.xta" >| ./scrypt/tmp2

        if grep -q "is satisfied" ./scrypt/tmp2; then
            nb_satisfied=$((nb_satisfied+1))
        fi
    done
    echo "nb satisfied: $nb_satisfied"
}

display_help () {
    echo "Usage : ./benchmark.sh [-v] [-t type] [-p type] [-f type] [-n number] [-l number]"
    echo "Options:"
    echo "-v : verify the worst-case response time of the systems."
    echo "-t type : specify which scheduability test to apply if wanted on the generated systems (test1)."
    echo "-s type : specify the type of system to generate (waters, waters_bis, waters1, waters2, waters_sag)."
    echo "-p type : specify the type of priority to apply (random, rate_monotonic)."
    echo "-f type : specify the method thatwill be used to generate the wcet factors (random, fixed)."
    echo "-d type : specify the distribution of the runnables among the tasks (random, fixed)."
    echo "-n number : specify the number of systems to generate."
    echo "-l number : specify the time limit for the schedulability analysis in seconds."
    exit 1
}

check_wcrt=0
check_test="none"
sys_type="waters1"
prio_type="random"
fact_type="fixed"
dist_type="fixed"
set_size=100
time_limit="3600s"


if [ $# -gt 15 ]; then
    echo "Too many arguments."
    display_help
fi

while getopts "hvs:t:p:f:d:n:l:" opt; do
    case $opt in
        h) 
            display_help ;;

        v)
            if [[ "${!OPTIND:0:1}" != "-" && -n "${!OPTIND}" ]]; then
               display_help
            fi
            check_wcrt=1
            ;;

        t) 
            if [ $OPTARG == "test1" ]; then
                sys_type=$OPTARG
            else
                display_help
            fi ;;

        s) 
            if [ $OPTARG == "waters" ] || [ $OPTARG == "waters_bis" ] || [ $OPTARG == "waters1" ] ||
                [ $OPTARG == "waters2" ] || [ $OPTARG == "waters_sag" ]; then
                sys_type=$OPTARG
            else
                display_help
            fi ;;

        p) 
            if [ $OPTARG == "random" ] || [ $OPTARG == "rate_monotonic" ] ; then
                prio_type=$OPTARG
            else
                display_help
            fi ;;

        f) 
            if [ $OPTARG == "random" ] || [ $OPTARG == "fixed" ] ; then
                fact_type=$OPTARG
            else
                display_help
            fi ;;

        d)
            if [ $OPTARG == "random" ] || [ $OPTARG == "fixed" ] ; then
                dist_type=$OPTARG
            else
                display_help
            fi ;;

        n)
            if ! [[ $OPTARG =~ ^[0-9]+$ ]]; then
                display_help
            fi     
            if [ $OPTARG -lt 1 ]; then
                display_help
            fi       
            set_size=$OPTARG ;;
        l)
            if ! [[ $OPTARG =~ ^[0-9]+$ ]]; then
                display_help
            fi     
            if [ $OPTARG -lt 1 ]; then
                display_help
            fi       
            time_limit="${OPTARG}s" ;;
        \?) 
            display_help
    esac
done

echo "starting the benchmark..."
for i in {1..9}; do
    j=$(echo "scale=1; $i / 10" | bc)
    ./generate $set_size $sys_type $prio_type $fact_type $dist_type $j $check_test 

    if [ $check_test == "none" ]; then 
        rm -rf ./systems_generation/system_files/systems.t
    fi

    echo "set $i with u = $j generated."
    gen_xta $check_wcrt
    
    output_file="results_$j.info"
    touch ./scrypt/$output_file

    evaluate_set $set_size $output_file $check_wcrt $time_limit
    echo "set $i evaluated."

    dir_results="results_$j"
    mkdir ./systems_generation/system_files/$dir_results
    mv ./systems_generation/system_files/system* ./systems_generation/system_files/$dir_results
 
done

echo "benchmark finished."
echo $(($set_size * 10)) "systems have been generated."

echo "analyzing the results..."
cd ./scrypt
python3 analyze.py
cd ..
echo "results analyzed."
