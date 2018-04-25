#!/bin/bash

expenses=("default" "flight" "fuels" "telecom")
distances=("JS" "cosine" "robust")
ks=(2 3 4 5)
legislatures=(53 54 55)

for expense in ${expenses[@]}; do
	for distance in ${distances[@]}; do
		for k in ${ks[@]}; do
			for legislature in ${legislatures[@]}; do
				source_file="../data/$expense/graphs/$distance/k-$k/cibm-regioncolor-$legislatures.graphml"
				target_path="../../vfpimenta.github.io/data/graph-json/standard/$expense/$distance/k-$k"
				target_file="cibm-legislature-$legislature.json"

				mkdir -p $target_path

				python3 graphml2json.py --source $source_file --target $target_path/$target_file
			done
		done
	done
done