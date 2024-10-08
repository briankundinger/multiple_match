#!/bin/bash
#SBATCH -p compsci
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=ncvr_fabl_swap
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=64G
#
# modules
#

Rscript code/swap/ncvr_fabl.R
