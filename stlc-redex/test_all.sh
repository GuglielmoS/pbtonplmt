#!/bin/bash

# control how many times each check must be run
num_repetitions=10

declare -A config

config["feat-auto"]="bug1#progress_13
bug1#preservation_13
bug2#progress_13
bug3#progress_13
bug3#preservation_13
bug5#preservation_13
bug6#progress_13
bug7#progress_13
bug8#preservation_13
bug9#preservation_13"

config["feat-hw"]="bug1#progress_10
bug1#preservation_10
bug2#progress_10
bug3#progress_10
bug3#preservation_10
bug5#preservation_10
bug6#progress_10
bug7#progress_10
bug8#preservation_10
bug9#preservation_10"

config["smallcheck-auto"]="bug1#progress_3
bug2#progress_3
bug3#progress_3
bug3#preservation_5
bug7#progress_6"

config["smallcheck-hw"]="bug1#progress_2
bug2#progress_2
bug3#progress_2
bug3#preservation_3
bug7#progress_4
bug8#preservation_3"

# WARNING: LSC doesn't consider the information about depth (i.e. 5)
config["lazysmallcheck-hw"]="bug1#progress_5
bug1#preservation_5
bug2#progress_5
bug3#progress_5
bug3#preservation_5
bug4#progress_5
bug5#preservation_5
bug6#progress_5
bug7#progress_5
bug8#preservation_5
bug9#preservation_5"

# WARNING: QC doesn't consider the information about depth (i.e. 1)
config["quickcheck-hw"]="bug1#progress_1
bug1#preservation_1
bug2#progress_1
bug3#progress_1
bug3#preservation_1
bug7#progress_1
bug8#preservation_1
bug9#preservation_1"

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
            current_timings+=$(stack exec stlc-redex-$tool $property $depth | grep "TIME:" | cut -f 2 -d :)
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
