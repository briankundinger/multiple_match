#!/bin/bash
#SBATCH -p compsci
#SBATCH --array=1
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=get_xtmp
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=16G
#
# modules
#

cp ../../../../../usr/xtmp/bak47/mm/swap/Z_hat/fabl out/xtmp/fabl_swap
cp ../../../../../usr/xtmp/bak47/mm/ncvr_b_dedupe/Z_hat/fabl_mm_inf out/xtmp/fabl_mm
cp ../../../../../usr/xtmp/bak47/mm/ncvr_b_dedupe/Z_hat/fabl out/xtmp/fabl