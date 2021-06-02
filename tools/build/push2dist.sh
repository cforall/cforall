#!/bin/bash

hash="$1"
bwlim="$2"
valid=$(distcc -j 2> /dev/null)
# if test "${valid}" != 0

hosts_long=$(distcc --show-hosts 2> /dev/null)
hosts="localhost"
while read -r host; do
	if [[ "${host}" =~ @?([A-Za-z0-9.\-]+)/.* ]]
	then
		hosts="${BASH_REMATCH[1]} ${hosts}"
	else
		>&2 echo "Could not parse host '${host}' (must use ssh hosts)"
		exit 1
	fi
done <<< "${hosts_long}"

# echo "Copying to machines : ${hosts} (hash=${hash})"

files="../../../driver/cfa ../../../driver/cfa-cpp ../../../driver/cc1 ../../../driver/as defines.hfa $(find . -name '*.c*' | tr '\n' ' ')"
# echo "Files ${files}"

function push() {
	ssh ${host} "mkdir -p ~/.cfadistcc/${hash}/"
	rsync --bwlimit=${bwlim} -a ${dV} ${files} ${host}:~/.cfadistcc/${hash}/.
}

for host in ${hosts}
do
	push &
done

wait
