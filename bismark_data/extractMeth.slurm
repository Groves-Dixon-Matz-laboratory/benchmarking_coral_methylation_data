#!/bin/bash
#SBATCH -J extractMeth
#SBATCH -n 8
#SBATCH -N 2
#SBATCH -p normal
#SBATCH -o extractMeth.o%j
#SBATCH -e extractMeth.e%j
#SBATCH -t 06:00:00
#SBATCH -A mega2014
#SBATCH --mail-type=ALL
#SBATCH --mail-user=grovesdixon@gmail.com

module load launcher





export LAUNCHER_PLUGIN_DIR=$LAUNCHER_DIR/plugins
export LAUNCHER_RMI=SLURM
export LAUNCHER_JOB_FILE=extractMeth

$LAUNCHER_DIR/paramrun
