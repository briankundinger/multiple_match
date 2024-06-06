#!/bin/bash
#SBATCH -p compsci
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=ncvr_combine_b_dedup
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=200G
#
# modules
#

Rscript code/b_dedup/ncvr_combine.R
