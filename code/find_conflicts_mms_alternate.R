library(dplyr)
library(parallel)
library(vabldev)

cores <- parallel::detectCores()

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# out <- readRDS("out/ncvr_results/chain/fabl_mm_2")
# hash <- readRDS("out/ncvr/combine/hash")
# result <- estimate_links_mm(out, hash, resolve = F, transitivity = F)
#
# Z_hat <- results$Z_hat
# prob <- results$prob

Z_hat <- readRDS("out/ncvr_results/Z_hat/fabl_mm_2_raw")
prob <- readRDS("out/ncvr_results/Z_hat/fabl_mm_2_raw")

Z_hat <- data.frame(Z_hat, prob)


mms_df <- Z_hat %>%
  group_split(base_id, .keep = T)

# mms_df[[201]] <- data.frame(target_id = 25,
#                             base_id = 201,
#                             prob = .9)

mms <- mms_df %>%
  lapply(., `[[`, "target_id")

mms_prob <- mms_df %>%
  lapply(., `[[`, "prob")

unique_mms <- unique(mms)
n_mms <- length(unique_mms)
unique_mms_map <- match(mms, unique_mms)

batch_size <- 200
normal_batches <- n_mms %/% batch_size
last_batch <- n_mms %% batch_size

batch_id <-c(rep(1:normal_batches, each = batch_size), rep(normal_batches + 1, last_batch))
set_vec <- seq(1:n_mms)[batch_id == k]

identify_conflicts <- function(set, mms){
  common_entities <- sapply(seq_along(unique_mms), function(j){
    intersect(set, unique_mms[[j]]) %>%
      length()
  })
  max(0, which(common_entities > 0 & common_entities < length(set)))
}

# conflicts <- parallel::mclapply(unique_mms[set_vec],
#                                 identify_conflicts,
#                                 unique_mms, mc.cores = cores)

conflicts <- list()
for(i in seq_along(set_vec)){
  print(i)
  conflicts[[i]] <- identify_conflicts(unique_mms[[set_vec[i]]], unique_mms)
}




saveRDS(conflicts, paste0("out/ncvr/mms/conflicts_alternate/", "conflicts_",
                    stringr::str_pad(k, 4, pad = "0")))


