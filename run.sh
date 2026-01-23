#!/bin/bash
#SBATCH --job-name=targets_main
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem 8G
#SBATCH --time=168:00:00
#SBATCH --partition=standard
#SBATCH -o %x_%j.out

# Load necessary modules
module load R

# Start targets pipeline
R -e 'targets::tar_make(reporter="verbose")'
