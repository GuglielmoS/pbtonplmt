#!/bin/bash

# control how many times each check must be run
num_repetitions=10

declare -A config

config["feat-auto"]="bug1#confinement_18"

config["smallcheck-auto"]="bug1#confinement_3
bug1#non-interference_4
bug2#non-interference_3"

config["lazysmallcheck-hw"]="bug2#non-interference_3"

# rebuild all if needed
stack build

# add the CSV header
header="tool, bug#property, depth/size, "
for i in `seq 1 $num_repetitions`
do
    header+="run#$i"
    if [ $i -lt $num_repetitions ]
    then header+=", "
    fi
done
header+=$'\n'

# execute the benchmark
timings=$header
for tool in "${!config[@]}"
do
    for conf in ${config[$tool]}
    do
        current_timings=""
        property=$(echo $conf | cut -f 1 -d _)
        depth=$(echo $conf | cut -f 2 -d _)

        for i in `seq 1 $num_repetitions`
        do
            current_timings+=$(stack exec imp-stc-$tool $property $depth | grep "TIME:" | cut -f 2 -d :)
            if [ $i -lt $num_repetitions ]
            then
               current_timings+=", "
            fi
        done
        timings+="$tool, $property, $depth, $current_timings"
        timings+=$'\n'
    done
done

echo "$timings" > timings_backup
echo "$timings" | python3 process_timings.py
