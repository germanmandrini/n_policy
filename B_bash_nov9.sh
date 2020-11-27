#!/bin/bash
#PBS -l nodes=1:ppn=28 ## Specify how many nodes/cores per node
##PBS -l walltime=00:01:00
#PBS -q secondary ## Specify which queue to run in

## cd /projects/aces
cd /home/germanm2/scratch
module load singularity ## Load the singularity runtime to your environment

# cell_n=$1 #split the cell string for bash work
cell_n=${cell_n} #for qsub

bash_n=129

## Code to get the cell_n from qsub or from bash
## if [ -z ${cell+x} ]; then echo "var is unset"; else echo "var is set to '$cell'"; fi
## if [ -z ${cell+x} ]; then cell_n=$1; cell_n=${cell_n};fi

echo "$cell_n"

singularity exec /home/germanm2/scratch/apsim_nov16.simg Rscript /home/germanm2/scratch/n_policy_git/Codes/simA_manager.R $cell_n $bash_n



