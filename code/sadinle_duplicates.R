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
S = 1000
burn = S * .1
show_progress = F
fast = F
R = NULL
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

Z_true_pairs <- data.frame(id_1 = 1:(2*overlap),
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

file1[paste_index, ] <- file1[copy_index, ]

cd <- compare_records(file1, file2, c(2, 3, 4, 5, 6) + 1,
                      types = c("lv", "lv", "bi", "bi", "bi"),
                      breaks = c(0, 0.25))

hash <- hash_comparisons(cd)

# fabl
# start <- proc.time()[3]
# out <- fabl(hash, S = S, burn = burn)
# time <- proc.time()[3] - start
# result <- estimate_links(out, hash, resolve = F)
# Z_hat <- make_Zhat_pairs(result$Z_hat)
# fabl_result <- c(evaluate_links(Z_hat, Z_true_pairs, n1, "pairs"), time)

# Multiple match
start <- proc.time()[3]
out_mm <- fabl_mm(hash, S = S, burn = burn, max_K = 2)
time <- proc.time()[3] - start
result_mm <- estimate_links_mm(out_mm, hash, resolve = T)
Z_hat <- cbind(result_mm$Z_hat$target_id, result_mm$Z_hat$base_id)
fabl_mm_result <- c(evaluate_links(Z_hat, Z_true_pairs, n1, "pairs"), time)

start <- proc.time()[3]
out_mm <- fabl_mm(hash, S = S, burn = burn)
time <- proc.time()[3] - start
result_mm <- estimate_links_mm(out_mm, hash, resolve = T)
Z_hat <- cbind(result_mm$Z_hat$target_id, result_mm$Z_hat$base_id)
fabl_mm_inf_result <- c(evaluate_links(Z_hat, Z_true_pairs, n1, "pairs"), time)

# Var Fastlink

# start <- proc.time()[3]
# out_fl <- variational_fastlink(hash, tmax = 500)
# time <- proc.time()[3] - start
# estimate_fl <- estimate_links_fl(out_fl, hash)
# Z_hat <- data.frame(id_1 = estimate_fl$fs_linkages$a,
#                     id_2 = estimate_fl$fs_linkages$b)
# var_fastlink_result <- c(evaluate_links(Z_hat, Z_true_pairs, n1, "pairs"), time)

# fastLink
start <- proc.time()[3]
fl_out <- fastLink::fastLink(file1, file2, varnames = names(file1)[c(2, 3, 4, 5, 6) + 1],
                             stringdist.match = names(file1)[c(2, 3) + 1],
                             partial.match = names(file1)[c(2, 3) + 1],
                             stringdist.method = "lv",
                             cut.a = 1, cut.p = .75, dedupe.matches = F, threshold.match = .5,
                             n.cores = 1, verbose = F, return.all = F, tol.em = 1e-07)
time <- proc.time()[3] - start

Z_hat <- data.frame(id_1 = fl_out$matches$inds.a,
                    id_2 = fl_out$matches$inds.b)

fastlink_result <- c(evaluate_links(Z_hat, Z_true_pairs, n1, "pairs"), time)

result_df <- rbind(fabl_mm_result, fabl_mm_inf_result, fastlink_result) %>%
  data.frame()


names(result_df) <- c("recall", "precision", "f-measure", "time")
#result_df$method <- c("fabl_mm", "fabl_mm_inf", "variational_fastlink", "fastlink", "multilink_2")
result_df$method <- c("fabl_mm", "fabl_mm_inf", "fastlink")


result_df$errors <- ceiling(i/100)
result_df$sim_number <- i
result_df$overlap <- overlap
result_list[[j]] <- result_df
}

result_final <- do.call(rbind, result_list)


saveRDS(result_final, file = paste0("out/sadinle_sim/sim_",
                                 str_pad(i, 3, pad = "0")))
