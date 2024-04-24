#!/bin/bash
#SBATCH --array=1
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=ncvr_combine
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=512G
#
# modules
#

Rscript code/ncvr_combine.R
