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
methods <- c("vabl", "DRL", "fastLink")

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

  cd <- compare_records(file_A, file_B, c(4, 5, 7, 8, 9),
                              types = c("lv", "lv", "bi", "bi", "bi"),
                              breaks = c(0, 0.25))

  hash <- hash_comparisons(cd)

  start <- proc.time()[3]
  out_mm <- vabl(hash)
  time <- proc.time()[3] - start
  result_mm <- estimate_links(out_mm, hash, resolve = T)
  Z_hat <- make_Zhat_pairs(result_mm$Z_hat)
  vabl_result <- c(evaluate_links(Z_hat, Z_true, n_A, "pairs"), time)

  start <- proc.time()[3]
  out_mm <- fabl_mm(hash, S = S, burn = burn)
  time <- proc.time()[3] - start
  result_mm <- estimate_links_mm(out_mm, hash, resolve = T, transitivity = F)
  Z_hat <- result_mm$Z_hat[, 1:2]
  #Z_hat <- make_Zhat_pairs(result_mm$Z_hat)
  drl_result <- c(evaluate_links(Z_hat, Z_true, n_A, "pairs"), time)

  start <- proc.time()[3]
  fl_out <- fastLink::fastLink(file_A, file_B, varnames = names(file_A)[c(4, 5, 6, 7, 8)],
                               stringdist.match = names(file_A)[c(4, 5, 6, 7, 8)],
                               partial.match = names(file_A)[c(4, 5)],
                               stringdist.method = "lv",
                               cut.a = 1, cut.p = .75, dedupe.matches = F, threshold.match = .5,
                               n.cores = 1, verbose = F, return.all = F, tol.em = 1e-07)
  time <- proc.time()[3] - start

  Z_hat <- data.frame(id_1 = fl_out$matches$inds.a,
                      id_2 = fl_out$matches$inds.b)
  fastlink_result <- c(evaluate_links(Z_hat, Z_true, n_A, "pairs"), time)


  df <- rbind(vabl_result, drl_result, fastlink_result) %>%
    data.frame() %>%
    mutate(duplication = dupe_rate[d],
           method = methods)

  df_list[[d]] <- df
}

final <- df_list %>%
  do.call(rbind, .)

#saveRDS(final, paste0("out/poisson/sim_", stringr::str_pad(j, 3, "left", "0")))
saveRDS(final, paste0("out/poisson_2/sim_", stringr::str_pad(j, 3, "left", "0")))
