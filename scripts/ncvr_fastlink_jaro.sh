#!/bin/bash
#SBATCH -p compsci-gpu
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=ncvr_fastlink_jaro
#SBATCH --time=24:00:00
#SBATCH --mem=600G
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24

#
# modules
#

Rscript code/ncvr_fastlink_jaro.R
