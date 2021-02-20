#!/bin/bash --login
#SBATCH --job-name=mini.hydro
#SBATCH --output=mini.hydro.out.%J
#SBATCH --error=mini.hydro.err.%J 
#SBATCH --partition=C1Mitad1
#SBATCH --ntasks-per-node=12
#SBATCH --ntasks=12
#SBATCH --mem=0  
#SBATCH --time=240:00:00

################################################
NNODES=$SLURM_NNODES
NCPUS=$SLURM_NTASKS
PPN=$SLURM_NTASKS_PER_NODE

echo Running on host `hostname`
echo Time is `date`
echo Directory is `pwd`
echo SLURM job ID is $SLURM_JOBID
echo This jobs runs on the following machine: `echo $SLURM_JOB_NODELIST | uniq`

echo Number of Processing Elements is $NCPUS
echo Number of mpiprocs per node is $PPN
################################################

env

script="Combinado.R"
#script="1-MiniHidroPowerSinMascaras4.R"


WDPATH=/home/$USER/

time Rscript ${script}
