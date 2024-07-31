library(stringr)
library(vabldev)
library(purrr)
library(readr)


taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
j = taskID
set.seed(41)
S = 1000
burn = ceiling(S * .1)

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

  # cd <- compare_records(file_A, file_B, c(4, 5, 6, 7, 8),
  #                             types = c("lv", "lv", "bi", "bi", "bi"),
  #                             breaks = c(0, 0.25))

  names(file_A)

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

  prior <- multilink::specify_prior(cd_multilink, mus = NA,
                                    nus = NA, flat = 0, alphas = NA,
                                    dup_upper_bound = c(1, 5),
                                    dup_count_prior_family = NA,
                                    dup_count_prior_pars = NA,
                                    n_prior_family = NA,
                                    n_prior_pars = NA)

  start <- proc.time()[3]
  chain_multilink <- multilink::gibbs_sampler(cd_multilink, prior, n_iter = S)
  time <- proc.time()[3] - start
  result_ML <- multilink::find_bayes_estimate(chain_multilink$partitions, burn)

  cluster_labels <- unique(result_ML)

  df_1_clusters <- result_ML[1:500]
  df_2_clusters <- result_ML[501:1000]

  Z_list <- list()

  for(x in cluster_labels){
    match1 <- which(df_1_clusters == x)
    match2 <- which(df_2_clusters == x)

    if(length(match1) == 0 ||length(match2) == 0){
      next
    }
    Z_list[[x]] <- data.frame(id_1 = match1,
                              id_2 = match2)
  }

  Z_hat <- do.call(rbind, Z_list)
  Z_hat <- Z_hat[, c(2, 1)]

  multilink_result <- c(evaluate_links(Z_hat, Z_true, n_A, "pairs"), time, d)

  df_list[[d]] <- multilink_result
}

final <- df_list %>%
  do.call(rbind, .)
#saveRDS(final, paste0("out/poisson_ml/sim_", stringr::str_pad(j, 3, "left", "0")))
saveRDS(final, paste0("out/poisson_2_ml/sim_", stringr::str_pad(j, 3, "left", "0")))
