#!/usr/bin/env bash
#SBATCH --job-name=centinela
#SBATCH --time=160:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=30G
#SBATCH --partition=hbfraser,hns

# module load java
# module load viz
# module load graphviz
module load R/4.1.2

date
echo "Starting pipeline"
Rscript casos/centinela_coronamex_bayes_seir.r
echo "Pipeline finished"
date
