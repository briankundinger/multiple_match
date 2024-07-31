library(stringr)
library(vabldev)
library(purrr)
library(readr)


taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
j = taskID
set.seed(41)
S = 1000
burn = S * .1

dupe_rate <- c("low", "mid", "high")
folder_names <- list.files("data/poisson_sims/", full.names = F)
df_list <- vector("list", length = length(dupe_rate))

for(d in seq_along(folder_names)){
  file_A <- read.csv(paste0("data/poisson_sims_2/",
                            folder_names[d],
                            "/sim_", stringr::str_pad(j, 3, "left", "0"),
                            "/file_A.csv"))
  file_B <- read.csv(paste0("data/poisson_sims_2/",
                            folder_names[d],
                            "/sim_", stringr::str_pad(j, 3, "left", "0"),
                            "/file_B.csv"))

  # file_A <- read.csv(paste0("data/poisson_sims/",
  #                           folder_names[d],
  #                           "/sim_", stringr::str_pad(j, 3, "left", "0"),
  #                           "/file_A.csv"))
  # file_B <- read.csv(paste0("data/poisson_sims/",
  #                           folder_names[d],
  #                           "/sim_", stringr::str_pad(j, 3, "left", "0"),
  #                           "/file_B.csv"))

  n_B <- nrow(file_B)
  n_A <- nrow(file_A)
  n2 <- n_B
  #with_matches <- n_B * .1
  with_matches <- n_B * .3
  dupes_A <- file_A %>%
    filter(rec.id <= with_matches) %>%
    select(rec.id) %>%
    pull()

  matched_B <- file_B$rec.id[dupes_A]
  matched_A <- file_A %>%
    filter(rec.id <= with_matches) %>%
    select(X) %>%
    pull()

  Z_true <- data.frame(target_id = matched_A,
                       base_id = matched_B)

  all_records <- rbind(file_B, file_A)[, c(4, 5, 7, 8, 9)]
  all_records$occup <- as.character(all_records$occup)
  cd_multilink <- multilink::create_comparison_data(all_records,
                                                    types = c("lv", "lv", "bi", "bi", "bi"),
                                                    breaks = list(c(0, .25),
                                                                  c(0, .25),
                                                                  NA,
                                                                  NA,
                                                                  NA),
                                                    file_sizes = c(n_B, n_A),
                                                    duplicates = c(0, 1),
                                                    verbose = T)

  keep <- (cd_multilink$comparisons[, "gname_DL_2"] != T) &
    (cd_multilink$comparisons[, "fname_DL_2"] != T)
  new_cd <- multilink::reduce_comparison_data(cd_multilink, keep, cc = 1)
  pairs_kept <- cd_multilink$record_pairs[keep, ]

  thing <- pairs_kept[pairs_kept[, 1] < 50, ]

  prior <- multilink::specify_prior(new_cd, mus = NA,
                                    nus = NA, flat = 0, alphas = NA,
                                    dup_upper_bound = c(1, 5),
                                    dup_count_prior_family = NA,
                                    dup_count_prior_pars = NA,
                                    n_prior_family = NA,
                                    n_prior_pars = NA)

  start <- proc.time()[3]
  chain_multilink <- multilink::gibbs_sampler(new_cd, prior, n_iter = S)
  time <- proc.time()[3] - start
  result_ML <- multilink::find_bayes_estimate(chain_multilink$partitions, burn)

  thing <- multilink::relabel_bayes_estimate(new_cd, result_ML)

  clusters <- data.frame(rec.id = c(1:500, 1:500), thing)
  clusters_B <- clusters[1:500, ]
  clusters_A <- clusters[501:1000, ]
  cluster_labels <- clusters %>%
    group_by(link_id) %>%
    count() %>%
    filter(n > 1) %>%
    select(link_id) %>%
    pull()

  Z_list <- list()

  for(x in cluster_labels){
    rec_B <- clusters_B$rec.id[which(clusters_B$link_id == x)]
    rec_A <- clusters_A$rec.id[which(clusters_A$link_id == x)]

    if(length(rec_B) == 0 ||length(rec_A) == 0){
      next
    }
    Z_list[[x]] <- data.frame(target_id = rec_A,
                              base_id = rec_B)
  }

  Z_hat <- do.call(rbind, Z_list)
  multilink_result <- c(evaluate_links(Z_hat, Z_true, n_A, "pairs"), time, d)

  df_list[[d]] <- multilink_result
}

final <- df_list %>%
  do.call(rbind, .)

#saveRDS(final, paste0("out/poisson_ml_filter/sim_", stringr::str_pad(j, 3, "left", "0")))
saveRDS(final, paste0("out/poisson_2_ml_filter/sim_", stringr::str_pad(j, 3, "left", "0")))
