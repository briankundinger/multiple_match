#!/bin/bash
#SBATCH -p compsci
#SBATCH --array=1-2241%150
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=ncvr_hash_b_dedup
#SBATCH --time=24:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16G

Rscript code/swap/ncvr_hash.R
