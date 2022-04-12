#!/bin/bash

#SBATCH --job-name=cbg__gHM_annotation
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ruth.oliver@yale.edu
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=100g -t 2-
#SBATCH --partition=general,bigmem,pi_jetz
#SBATCH -C avx2

module load R/4.1.0-foss-2020b

Rscript /gpfs/ysm/project/jetz/ryo3/projects/wi-coverage/src/poc/extract-gHM-cbg.R