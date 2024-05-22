library(dplyr)
library(parallel)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

mms <- readRDS("out/ncvr/mms/combine/mms")
prob <- readRDS("out/ncvr/mms/combine/prob")

unique_mms <- unique(mms)
n_mms <- length(unique_mms)
unique_mms_map <- match(mms, unique_mms)

batch_size <- 200
normal_batches <- n_mms %/% batch_size
last_batch <- n_mms %% batch_size

batch_id <-c(rep(1:normal_batches, each = batch_size), rep(normal_batches + 1, last_batch))
record_vec <- seq(1:n2)[batch_id == k]

identify_conflicts <- function(set, mms){
  common_entities <- sapply(seq_along(mms), function(j){
    intersect(set, mms[[j]]) %>%
      length()
  })
  max(0, which(common_entities > 0 & common_entities < length(set)))
}



#conflicts <- sapply(unique_mms, identify_conflicts, unique_mms)
conflicts <- parallel::mclapply(unique_mms[record_vec],
                                identify_conflicts,
                                unique_mms, mc.cores = 40)




saveRDS(mms, paste0("out/ncvr/mms/conflicts/", "conflicts_",
                    stringr::str_pad(k, 4, pad = "0")))
#
# saveRDS(mms_probs, paste0("out/ncvr/mms/prob_batch/", "prob_",
#                      stringr::str_pad(k, 4, pad = "0")))

