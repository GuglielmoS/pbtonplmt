#!/usr/bin/python

import csv
import sys
import collections

timings = collections.OrderedDict()
header = ""
csvreader = csv.reader(sys.stdin, delimiter=',')
for row in csvreader:
    # take the header first
    if header == "":
        header = row[:]
    elif len(row) > 0:
        tool = row[0].strip()
        bug_and_property = row[1].strip()
        depth_or_size = row[2].strip()
        runs = [int(timing) for timing in row[3:]]

        if not tool in timings:
            timings[tool] = collections.OrderedDict()
        if not bug_and_property in timings[tool]:
            timings[tool][bug_and_property] = []
 
        timings[tool][bug_and_property] = [tool, bug_and_property, depth_or_size, runs, sum(runs)/len(runs)]

print(', '.join(header) + ", average")
for tool,bugs_and_properties in timings.items():
    for bug_and_property, results in bugs_and_properties.items():
        res = []
        res.append(str(results[0]))
        res.append(str(results[1]))
        res.append(str(results[2]))
        res += [str(run) for run in results[3]]
        res.append(str(results[4]))
        print(', '.join(res))
