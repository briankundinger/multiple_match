library(dplyr)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

hash <- readRDS("out/ncvr/combine/hash")
out <- readRDS("out/ncvr_results/chain/fabl_mm_2")


n1 <- hash$n1
n2 <- hash$n2
Z_samps <- out$Z
samps <- ncol(Z_samps)

batch_size <- 100
normal_batches <- n2 %/% batch_size
last_batch <- n2 %% batch_size

batch_id <-c(rep(1:normal_batches, each = batch_size), rep(normal_batches + 1, last_batch))
record_vec <- seq(1:n2)[batch_id == k]

mms <- vector("list", n2)
mms_probs <- vector("double", n2)

for(j in seq_along(record_vec)){
  record <- record_vec[j]

  samples <- Z_samps[record, , ] %>%
    apply(., 1, sort, simplify = F)

  unique_sets <- samples %>%
    unique()

  prob <- match(samples, unique_sets) %>%
    table(.)/ samps

  mms[[j]] <- unique_sets[[which.max(prob)]]
  mms_probs[j] <- max(prob)
}

saveRDS(mms, paste0("out/ncvr/mms/mms_batch/", "mms_",
                     stringr::str_pad(k, 4, pad = "0")))

saveRDS(mms_probs, paste0("out/ncvr/mms/prob_batch/", "prob_",
                     stringr::str_pad(k, 4, pad = "0")))

