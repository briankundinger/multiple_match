library(dplyr)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

hash <- readRDS("out/ncvr/combine/hash")
mms <- readRDS("out/ncvr/mms/combine/mms")
mms_prob <- readRDS("out/ncvr/mms/combine/prob")
Z_true <- readRDS("data/ncvr_Z_true")
names(Z_true) <- c("target_id", "base_id")

Z_true %>%
  filter(base_id == 625)
unique_mms[150067]


files <- list.files("out/ncvr/mms/conflicts/", full.names = T)
conflicts <- lapply(files, readRDS) %>%
  purrr::flatten()

unique_mms <- unique(mms)
unique_mms_map <- match(mms, unique_mms)

if(any(conflicts >0)){
  conflicts_df <- data.frame(code_1 = unlist(conflicts)) %>%
    mutate(code_2 = row_number()) %>%
    filter(code_1 > 0)

  for(i in seq_len(nrow(conflicts_df))){
    higher_prob <- data.frame(set = unique_mms_map,
                              probs = mms_prob) %>%
      mutate(record = row_number()) %>%
      filter(set %in% conflicts_df[i, ]) %>%
      filter(probs == max(probs)) %>%
      # mutate(size = sapply(mms[record], length)) %>%
      # mutate(total_prob = probs * size) %>%
      # filter(total_prob == max(total_prob)) %>%
      select(set) %>%
      pull()

    lower_prob <-  conflicts_df[i, ][conflicts_df[i, ] != higher_prob]
    mms[which(unique_mms_map %in% lower_prob)] <- 0
    print(i)
  }
}

Z_hat <- lapply(seq_along(mms), function(j){
  data.frame(target_id = mms[[j]],
             base_id = j,
             prob = mms_prob[j])
}) %>%
  do.call(rbind, .) %>%
  filter(target_id != 0)

probs_matches <- Z_hat %>%
  select(prob)

Z_hat <- Z_hat %>%
  select(target_id, base_id)



eval <- evaluate_links(Z_hat, Z_true, hash$n1, "pairs")


saveRDS(eval, "out/ncvr_results/eval/fabl_mm_2_mms_resolve")
saveRDS(Z_hat, "out/ncvr_results/Z_hat/fabl_mm_2_mms_resolve")
saveRDS(probs_matches, "out/ncvr_results/prob/fabl_mm_2_mms_resolve")
