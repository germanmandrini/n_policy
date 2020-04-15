#!/bin/bash
#PBS -l nodes=1:ppn=4 ## Specify how many nodes/cores per node
##PBS -l walltime=00:01:00
#PBS -q secondary ## Specify which queue to run in

cd /projects/aces
module load singularity ## Load the singularity runtime to your environment
# cell_n=$1 #split the cell string for bash work
cell_n=${cell_n} #for qsub

echo "$cell_n"

singularity exec /projects/aces/germanm2/apsim_nov16.simg Rscript /projects/aces/germanm2/n_policy_git/Codes/1_daily_to_yearly_nov26.R $cell_n



