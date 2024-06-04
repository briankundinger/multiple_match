library(dplyr)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

Z_hat <- readRDS("out/ncvr_results/Z_hat/fabl_mm_2_raw")
prob <- readRDS("out/ncvr_results/prob/fabl_mm_2_raw")
Z_hat <- data.frame(Z_hat, prob)

Z_true <- readRDS("data/ncvr_Z_true")
names(Z_true) <- c("target_id", "base_id")

Z_true %>%
  filter(base_id == 24399)
#
# mms_df <- Z_hat %>%
#   group_split(base_id, .keep = T)

mms <- mms_df %>%
  lapply(., `[[`, "target_id")

mms_prob <- mms_df %>%
  lapply(., `[[`, "prob")


unique_mms <- unique(mms)
unique_mms_map <- match(mms, unique_mms)

set_id_df <- lapply(seq_along(mms), function(j){
  data.frame(mms_df[[j]], set_id = unique_mms_map[j])
}) %>%
  do.call(rbind, .)


files <- list.files("out/ncvr/mms/conflicts_alternate/", full.names = T)
conflicts <- lapply(files, readRDS) %>%
  purrr::flatten()
conflicts_df <- lapply(seq_along(conflicts), function(x){
  data.frame(set_1 = conflicts[[x]], set_2 = x)
}) %>%
  do.call(rbind, .) %>%
  filter(set_1 >0)

# ncvr_b %>%
#   mutate(rn = row_number()) %>%
#   filter(rn == 432)
#
# ncvr_a %>%
#   mutate(rn = row_number()) %>%
#   filter(rn == 88512)

#if(any(conflicts >0)){
if(nrow(conflicts_df > 0)){
  # conflicts_df <- data.frame(code_1 = conflicts) %>%
  #   mutate(code_2 = row_number()) %>%
  #   filter(code_1 > 0)

  for(i in seq_len(nrow(conflicts_df))){
    higher_prob <- set_id_df %>%
      filter(set_id %in% conflicts_df[i, ]) %>%
      group_by(base_id) %>%
      mutate(avg_prob = mean(prob)) %>%
      ungroup() %>%
      filter(avg_prob == max(avg_prob)) %>%
      select(set_id) %>%
      pull() %>%
      unique()

    lower_prob <-  conflicts_df[i, ][conflicts_df[i, ] != higher_prob]

    set_id_df <- set_id_df %>%
      filter(!(set_id %in% lower_prob))
    print(i)
    # mms[[which(unique_mms_map ==  lower_prob)]] <- 0
    # mms_prob[[which(unique_mms_map ==  lower_prob)]] <- 0
  }
}
}

# Z_hat <- lapply(1:n2, function(j){
#   data.frame(target_id = mms[[j]],
#              base_id = j,
#              prob = mms_prob[j])
# }) %>%
#   do.call(rbind, .) %>%
#   filter(target_id != 0)

Z_hat <- set_id_df %>%
  select(target_id, base_id)

probs_matches <- set_id_df %>%
  select(prob)

eval <- evaluate_links(Z_hat, Z_true, hash$n1, "pairs")

saveRDS(eval, "out/ncvr_results/eval/fabl_mm_2_sad_bayes")
saveRDS(Z_hat, "out/ncvr_results/Z_hat/fabl_mm_2_sad_bayes")
saveRDS(probs_matches, "out/ncvr_results/prob/fabl_mm_2_sad_bayes")
