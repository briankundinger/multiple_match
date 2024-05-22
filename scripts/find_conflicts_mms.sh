#!/bin/bash
#SBATCH -p compsci
#SBATCH --array=1-758%100
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=find_conflicts_mms
#SBATCH --time=24:00:00
#SBATCH --ntasks=1


Rscript code/find_conflicts_mms.R
