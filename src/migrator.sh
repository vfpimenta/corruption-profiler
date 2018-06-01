#!/bin/bash
 
expenses=("flight" "publicity" "telecom" "fuels" "maintenance" "consultancy" "auto-watercraft" "auto" "postal" "flight-ticket" "lodging" "meal" "aircraft" "security" "locomotion" "taxi" "publication" "software" "office" "watercraft" "maritme" "course" "default")
distances=("JS" "cosine" "robust")
ks=(2 3 4 5)
legislatures=(53 54 55)

for expense in ${expenses[@]}; do
	for distance in ${distances[@]}; do
		for k in ${ks[@]}; do
			for legislature in ${legislatures[@]}; do
				source_file="../data/$expense/graphs/$distance/k-$k/cibm-regioncolor-$legislature.graphml"
				target_path="../../vfpimenta.github.io/data/graph-json/standard/$expense/$distance/k-$k"
				target_file="cibm-legislature-$legislature.json"

				mkdir -p $target_path

				echo "Running for ($expense,$distance,$k,$legislature)"
				python3 graphml2json.py --source $source_file --target $target_path/$target_file
			done
		done
	done
done