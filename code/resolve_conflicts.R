library(dplyr)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

hash <- readRDS("out/ncvr/combine/hash")
out <- readRDS("out/ncvr_results/chain/fabl_mm_2")

files <- list.files("out/ncvr/mms/conflicts/", full.names = T)
conflicts <- lapply(files, readRDS)

thing <- readRDS(files[1])
