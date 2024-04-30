#!/bin/bash
#SBATCH -p compsci
#SBATCH --array=1-1001
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=array_test
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=1M

ls
