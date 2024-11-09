#!/bin/bash
if [ -e systems_generation/sag_files/system_1_.rta.csv ]; then
  rm systems_generation/sag_files/*.rta.csv
fi
for fic in systems_generation/sag_files/*.csv
do
  ./np-schedulability-analysis/build/nptest "$fic" -r
done