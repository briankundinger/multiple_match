library(vabldev)

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")
S <- 10
burn <- S * .1
tmax = 1000

df1 <- ncvr_a %>%
  select(voter_id) %>%
  mutate(rn = row_number())
# %>%
#   arrange(voter_id)

df2 <- ncvr_b %>%
  select(voter_id) %>%
  mutate(rn = row_number())
#%>% arrange(voter_id)

n1 <- nrow(df1)
n2 <- nrow(df2)

joined <- right_join(df1, df2, by = "voter_id", copy = T, keep = T,
                     relationship = "many-to-many") %>%
  arrange(voter_id.y)

Z_true_pairs <- joined %>%
  filter(!is.na(voter_id.x)) %>%
  select(rn.x, rn.y)
# joined$rn[is.na(joined$rn)] <- 0
# Z_true <- joined$rn

hash <- readRDS("out/ncvr/combine/hash")

ptm <- proc.time()
chain <- fabl(hash, S = S, burn = burn)
seconds <- proc.time() - ptm
results <- estimate_links(chain, hash)
eval <- evaluate_links(results$Z_hat, Z_true_pairs, n1, "pairs")
df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                      precision = eval[2],
                      f_measure = eval[3],
                      iterations = S,
                 time = seconds[3],
                      method = "fabl",
                      data = "ncvr")
saveRDS(df, "out/ncvr_results/fabl")

ptm <- proc.time()
chain <- fabl_mm(hash, S = S, burn = burn)
seconds <- proc.time() - ptm
results <- estimate_links(chain, hash)
eval <- evaluate_links(results$Z_hat, Z_true_pairs, n1, "pairs")
df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = S,
                 time = seconds[3],
                 method = "fabl",
                 data = "ncvr")
saveRDS(df, "out/ncvr_results/fabl_mm")

ptm <- proc.time()
out <- variational_fastlink(hash, tmax = tmax)
seconds <- proc.time() - ptm

files <- list.files(path = "out/ncvr/hash/", full.names = T)
file_1_labs <- 1:n1

batch_size <- 100

normal_batches <- n2 %/% batch_size
last_batch <- n2 %% batch_size
fs_matches <- list()

for(i in 1:length(files)){
  batch <- readRDS(files[i])


  batch_id <-c(rep(1:normal_batches, each = batch_size), rep(normal_batches + 1, last_batch))
  batch_labs <- df2[batch_id == i, ]$rn
  ids <- expand.grid(file_1_labs, batch_labs)
  fs_matches[[i]] <- data.frame(id_1 = ids[, 1],
             id_2 = ids[, 2],
             prob = out$fs_probs[batch$hash_id]) %>%
    filter(prob > .5)
}

fs_df <- do.call(rbind, fs_matches)

eval <- evaluate_links(fs_matches[, 1:2], Z_true_pairs, n1, "pairs")
df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = tmax,
                 time = seconds[3],
                 method = "fastlink",
                 data = "ncvr")
saveRDS(df, "out/ncvr_results/fastlink")

# trace_df <- data.frame(data = "ncvr",
#                        trace = chain$overlap) %>%
#   mutate(iter = row_number())
#
# saveRDS(trace_df, "out/case_study_trace/ncvr")


