#!/bin/bash
#SBATCH -J runBismark
#SBATCH -n 8
#SBATCH -N 2
#SBATCH -p normal
#SBATCH -o runBismark.o%j
#SBATCH -e runBismark.e%j
#SBATCH -t 10:00:00
#SBATCH -A mega2014
#SBATCH --mail-type=ALL
#SBATCH --mail-user=grovesdixon@gmail.com

module load launcher





export LAUNCHER_PLUGIN_DIR=$LAUNCHER_DIR/plugins
export LAUNCHER_RMI=SLURM
export LAUNCHER_JOB_FILE=runBismark

$LAUNCHER_DIR/paramrun
