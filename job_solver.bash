#!/bin/bash
#
#PBS -A GEO018
#PBS -N converter
#PBS -j oe
#PBS -o job_solver.$PBS_JOBID.o

###################################################

## USER PARAMETERS
## chester: gpu compute nodes have 1 GPU card (K20x) and 16-core (interlagos) CPU

#PBS -l walltime=1:00:00
#PBS -l nodes=1

## solver runs on GPUs
##PBS -l feature=gpu

NPROC=1

###################################################

cd $PBS_O_WORKDIR

echo "running converter: `date`"
echo "directory: `pwd`"
echo

# obtain job information
#cat $PBS_NODEFILE > OUTPUT_FILES/compute_nodes
#echo "$PBS_JOBID" > OUTPUT_FILES/jobid

# stores setup
#cp DATA/Par_file OUTPUT_FILES/
#cp DATA/CMTSOLUTION OUTPUT_FILES/
#cp DATA/STATIONS OUTPUT_FILES/

# runs simulation
echo
echo "running converter..."
echo `date`
aprun -n $NPROC ./sac_to_asdf

echo
echo "see results in directory: OUTPUT_FILES/"
echo
echo "done: `date`"


