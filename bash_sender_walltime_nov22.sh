#!/bin/bash

module load singularity

# singularity exec /projects/aces/germanm2/apsim_nov16.simg Rscript /projects/aces/germanm2/vr_value_v2/Codes/walltime_updater.R

cat /projects/aces/germanm2/n_policy_git/id_10_walltime.txt | while read i #each line has the cell and the estimated time to be runed

do
	cell_n="$(cut -d' ' -f1 <<<"$i")" #split the cell string
	walltime_n="$(cut -d' ' -f2 <<<"$i")" #split the time string
	echo $cell_n $walltime_n
	# ./apsim_bash.sh $i
	qsub -v cell_n=$cell_n -l walltime=00:$walltime_n:00 -N $cell_n ./apsim_bash_nov22.sh #send the cell_n as argument to the script and the walltime in the command line
	# qsub -v $i -N $i ./apsim_bash.sh
done
#qstat -u $USER

