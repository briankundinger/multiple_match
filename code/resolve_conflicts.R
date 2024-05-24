library(dplyr)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))


files <- list.files("out/ncvr/mms/conflicts_alternate/", full.names = T)
conflicts <- lapply(files, readRDS)
conflicts_list <- purrr::flatten(conflicts)
conflicts_df <- lapply(seq_along(conflicts), function(x){
  data.frame(set_1 = conflicts_list[[x]], set_2 = x)
}) %>%
  do.call(rbind, .) %>%
  filter(set_1 >0)

#if(any(conflicts >0)){
if(nrow(conflicts_df > 0)){
  # conflicts_df <- data.frame(code_1 = conflicts) %>%
  #   mutate(code_2 = row_number()) %>%
  #   filter(code_1 > 0)

  for(i in seq_len(nrow(conflicts_df))){
    higher_prob <- set_id_df %>%
      filter(set_id %in% conflicts_df[i, ]) %>%
      group_by(base_id) %>%
      mutate(total_prob = sum(prob)) %>%
      ungroup() %>%
      filter(total_prob == max(total_prob)) %>%
      select(set_id) %>%
      pull() %>%
      unique()

    lower_prob <-  conflicts_df[i, ][conflicts_df[i, ] != higher_prob]

    set_id_df <- set_id_df %>%
      filter(!(set_id %in% lower_prob))
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
