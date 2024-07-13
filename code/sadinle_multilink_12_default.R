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
S = 10
burn = S * .1
show_progress = F
fast = F
R = NULL
all_patterns = TRUE
tmax= 200
threshold = 1e-6
resolve = T
overlap_vec <- c(25, 125, 225)
result_list <- list()


for(j in seq_along(overlap_vec)){
  overlap <- overlap_vec[j]

records <- read_csv(files[i], col_types = cols())
records$file <- rep(2:1, length.out = dim(records)[1])

records <- records %>%
  janitor::clean_names() %>%
  mutate(rec_id = as.numeric(str_extract(rec_id, "\\d{3}")) + 1)

n1 <- 500
n2 <- 500

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

file1[paste_index, ] <- file1[copy_index, ]

Z_true_pairs <- data.frame(id_1 = 1:(2*overlap),
                           id_2 = rep(1:overlap, 2)) %>%
  arrange(id_2)

# copy_index <- 1:overlap
# paste_index <- (overlap +1):(2*overlap)
#
# file1[paste_index, ] <- file1[copy_index, ]
# file2[paste_index, ] <- file2[copy_index, ]
#
# Z_true_pairs <- data.frame(id_1 = rep(1:(2*overlap), 2),
#                           id_2 = c(rep(1:overlap, 2),
#                                    rep(1:overlap, 2) + overlap)) %>%
#   arrange(id_2)

#all_records <- rbind(file1, file2)[, c(2, 3, 5, 6) + 1]
all_records <- rbind(file1, file2)[, c(2, 3, 4, 5, 6) + 1]
all_records$occup <- as.character(all_records$occup)
cd_multilink <- multilink::create_comparison_data(all_records,
                                                  types = c("lv", "lv", "bi", "bi", "bi"),
                                                  breaks = list(c(0, .25),
                                                                c(0, .25),
                                                                NA,
                                                                NA,
                                                                NA),
                                                  file_sizes = c(n1, n2),
                                                  duplicates = c(1, 0),
                                                  verbose = T)

prior <- multilink::specify_prior(cd_multilink, mus = NA,
                                  nus = NA, flat = 0, alphas = NA,
                                  dup_upper_bound = NA,
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

multilink2_result <- c(evaluate_links(Z_hat, Z_true_pairs, n1, "pairs"), time)
result_list[[j]] <- multilink2_result
}

result_final <- do.call(rbind, result_list) %>%
  data.frame()
result_final$errors <- ceiling(i/100)
result_final$sim_number <- i
result_final$overlap <- overlap_vec



saveRDS(result_final, file = paste0("out/sadinle_sim_ml_12_default/sim_",
                                 str_pad(i, 3, pad = "0")))
