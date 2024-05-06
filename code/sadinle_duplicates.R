library(RecordLinkage)
library(dplyr)
library(stringr)
library(vabldev)
library(purrr)
library(readr)


taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
i = taskID
set.seed(41)
files <- list.files(path = "data/sadinle_sim_data/", full.names = T)

m_prior = 1
u_prior = 1
alpha = 1
beta = 1
S = 400
burn = S * .1
show_progress = F
fast = F
R = NULL
all_patterns = TRUE
tmax= 200
threshold = 1e-6
resolve = T

overlap <- 200

records <- read_csv(files[i], col_types = cols())
records$file <- rep(2:1, length.out = dim(records)[1])

records <- records %>%
  janitor::clean_names() %>%
  mutate(rec_id = as.numeric(str_extract(rec_id, "\\d{3}")) + 1)

n1 <- 500
n2 <- 500

Ztrue_pairs <- data.frame(id_1 = 1:(2*overlap),
                          id_2 = rep(1:overlap, 2)) %>%
  arrange(id_2)

file1 <- records %>%
  filter(file ==1,
         rec_id <= n1) %>%
  as.matrix(.) %>%
  data.frame(.) %>%
  mutate(occup = as.numeric(occup))

file2 <- records %>%
  filter(file == 2,
         rec_id %in% c(1:overlap, (n1 +1):(1000 - overlap))) %>%
  as.matrix() %>%
  data.frame(.) %>%
  mutate(occup = as.numeric(occup))

copy_index <- 1:overlap
paste_index <- (overlap +1):(2*overlap)

# copy_index <- 1:(overlap/2)
# paste_index <- (overlap +1):(overlap + overlap/2)

file1[paste_index, ] <- file1[copy_index, ]

cd <- compare_records(file1, file2, c(2, 3, 5, 6) + 1,
                      types = c("lv", "lv", "bi", "bi"))

                      #breaks = c(0, 0.25))

hash <- hash_comparisons(cd)

# fabl
# start <- proc.time()[3]
# out <- fabl(hash, S = S, burn = burn)
# time <- proc.time()[3] - start
# result <- estimate_links(out, hash, resolve = F)
# Z_hat <- make_Zhat_pairs(result$Z_hat)
# fabl_result <- c(evaluate_links(Z_hat, Ztrue_pairs, n1, "pairs"), time)

# Multiple match
start <- proc.time()[3]
out_mm <- fabl_mm(hash, S = S, burn = burn)
time <- proc.time()[3] - start
result_mm <- estimate_links_mm(out_mm, hash, resolve = F)
Z_hat <- cbind(result_mm$Z_hat$target_id, result_mm$Z_hat$base_id)
fabl_mm_result <- c(evaluate_links(Z_hat, Ztrue_pairs, n1, "pairs"), time)

# Fastlink

start <- proc.time()[3]
out_fl <- variational_fastlink(hash, tmax = 500)
time <- proc.time()[3] - start
estimate_fl <- estimate_links_fl(out_fl, hash)
Z_hat <- data.frame(id_1 = estimate_fl$fs_linkages$a,
                    id_2 = estimate_fl$fs_linkages$b)
fastlink_result <- c(evaluate_links(Z_hat, Ztrue_pairs, n1, "pairs"), time)

# MultiLink

# all_records <- rbind(file1, file2)[, c(2, 3, 5, 6) + 1]
# all_records$occup <- as.character(all_records$occup)
# cd_multilink <- multilink::create_comparison_data(all_records,
#                                                   types = c("lv", "lv", "bi", "bi"),
#                                                   breaks = list(c(0, .25, .5),
#                                                                 c(0, .25, .5),
#                                                                 NA,
#                                                                 NA),
#                                                   file_sizes = c(n1, n2),
#                                                   duplicates = c(1, 0),
#                                                   verbose = T)
#
# # One Match
#
# prior <- multilink::specify_prior(cd_multilink, NA, NA, 0,
#                                   NA, c(1, 1), NA, list(1, 1), NA, NA)
# start <- proc.time()[3]
# chain_multilink <- multilink::gibbs_sampler(cd_multilink, prior, n_iter = S)
# time <- proc.time()[3] - start
# result_ML <- multilink::find_bayes_estimate(chain_multilink$partitions, burn)
#
# cluster_labels <- unique(result_ML)
#
# df_1_clusters <- result_ML[1:500]
# df_2_clusters <- result_ML[501:1000]
#
# Z_list <- list()
#
# for(x in cluster_labels){
#   match1 <- which(df_1_clusters == x)
#   match2 <- which(df_2_clusters == x)
#
#   if(length(match1) == 0 ||length(match2) == 0){
#     next
#   }
#   Z_list[[x]] <- data.frame(id_1 = match1,
#                             id_2 = match2)
# }
#
# Z_hat <- do.call(rbind, Z_list)
#
# multilink1_result <- c(evaluate_links(Z_hat, Ztrue_pairs, n1, "pairs"), time)
#
# # Two Match
#
# prior <- multilink::specify_prior(cd_multilink, NA, NA, 0,
#                                   NA, c(2, 1), NA, list(1, 1), NA, NA)
#
# start <- proc.time()[3]
# chain_multilink <- multilink::gibbs_sampler(cd_multilink, prior, n_iter = S)
# time <- proc.time()[3] - start
# result_ML <- multilink::find_bayes_estimate(chain_multilink$partitions, burn)
#
# cluster_labels <- unique(result_ML)
#
# df_1_clusters <- result_ML[1:500]
# df_2_clusters <- result_ML[501:1000]
#
# Z_list <- list()
#
# for(x in cluster_labels){
#   match1 <- which(df_1_clusters == x)
#   match2 <- which(df_2_clusters == x)
#
#   if(length(match1) == 0 ||length(match2) == 0){
#     next
#   }
#   Z_list[[x]] <- data.frame(id_1 = match1,
#                             id_2 = match2)
# }
#
# Z_hat <- do.call(rbind, Z_list)
#
# multilink2_result <- c(evaluate_links(Z_hat, Ztrue_pairs, n1, "pairs"), time)
#
# # Three Match
#
# prior <- multilink::specify_prior(cd_multilink, NA, NA, 0,
#                                   NA, c(3, 1), NA, list(1, 1), NA, NA)
#
# start <- proc.time()[3]
# chain_multilink <- multilink::gibbs_sampler(cd_multilink, prior, n_iter = S)
# time <- proc.time()[3] - start
# result_ML <- multilink::find_bayes_estimate(chain_multilink$partitions, burn)
#
# cluster_labels <- unique(result_ML)
#
# df_1_clusters <- result_ML[1:500]
# df_2_clusters <- result_ML[501:1000]
#
# Z_list <- list()
#
# for(x in cluster_labels){
#   match1 <- which(df_1_clusters == x)
#   match2 <- which(df_2_clusters == x)
#
#   if(length(match1) == 0 ||length(match2) == 0){
#     next
#   }
#   Z_list[[x]] <- data.frame(id_1 = match1,
#                             id_2 = match2)
# }
#
# Z_hat <- do.call(rbind, Z_list)
#
# multilink3_result <- c(evaluate_links(Z_hat, Ztrue_pairs, n1, "pairs"), time)
# result_df <- rbind(fabl_result, fabl_mm_result, fastlink_result,
#                    multilink1_result, multilink2_result, multilink3_result) %>%
#   data.frame()
#
#
# names(result_df) <- c("recall", "precision", "f-measure", "time")
# result_df$method <- c("fabl", "fabl_mm", "fastlink", "multilink_1",
#                       "multilink_2", "multilink_3")

result_df <- rbind(fabl_mm_result, fastlink_result) %>%
  data.frame()


names(result_df) <- c("recall", "precision", "f-measure", "time")
result_df$method <- c("fabl_mm", "fastlink")

result_df$errors <- ceiling(i/100)
result_df$sim_number <- i


saveRDS(result_df, file = paste0("out/sadinle_sim/sim_",
                                 str_pad(i, 3, pad = "0")))
