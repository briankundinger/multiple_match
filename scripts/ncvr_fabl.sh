#!/bin/bash
#SBATCH -p compsci
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=ncvr_fabl
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=100G
#
# modules
#

Rscript code/ncvr_fabl.R
