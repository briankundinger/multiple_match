#!/bin/bash
#SBATCH -p compsci-gpu
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=ncvr_fastlink
#SBATCH --time=24:00:00
#SBATCH --mem=400G
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20

#
# modules
#

Rscript code/ncvr_fastlink.R
