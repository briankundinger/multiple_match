#!/bin/bash
#SBATCH -p compsci-gpu
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=mms_fabl_inf
#SBATCH --time=24:00:00

Rscript code/get_mms_Z_hat/fabl_mm_inf.R
