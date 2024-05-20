#!/bin/bash
#SBATCH -p compsci-gpu
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=mms_fabl_2
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=64G

Rscript code/get_mms_Z_hat/fabl_mm_2.R
