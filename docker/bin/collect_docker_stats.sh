#!/bin/bash

echo "CPU perc,Mem usage,Mem perc" >> docker_stats.csv
while true; do 
    docker stats --no-stream --format "{{.CPUPerc}},{{.MemUsage}},{{.MemPerc}}" $1 >> docker_stats.csv;
done
