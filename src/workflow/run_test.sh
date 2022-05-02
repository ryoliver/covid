#!/bin/bash

#SBATCH --job-name=test
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ruth.oliver@yale.edu
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=100g -t 2-
#SBATCH --partition=general,pi_jetz
#SBATCH -C avx2

module load conda
conda activate move

Rscript /gpfs/ysm/project/jetz/ryo3/projects/covid/src/poc/plot-examples.R