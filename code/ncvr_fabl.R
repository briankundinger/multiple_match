library(vabldev)

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")
S <- 5
burn <- ceiling(S * .1)
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
Z_hat <- make_Zhat_pairs(results$Z_hat)
saveRDS(Z_hat, "out/ncvr_results/Z_hat/fabl")
eval <- evaluate_links(Z_hat, Z_true_pairs, n1, "pairs")
df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                      precision = eval[2],
                      f_measure = eval[3],
                      iterations = S,
                 time = seconds[3],
                      method = "fabl",
                      data = "ncvr")
saveRDS(df, "out/ncvr_results/eval/fabl")

ptm <- proc.time()
chain <- fabl_mm(hash, S = S, burn = burn)
seconds <- proc.time() - ptm
results <- estimate_links(chain, hash)
Z_hat <- make_Zhat_pairs(results$Z_hat)
eval <- evaluate_links(Z_hat, Z_true_pairs, n1, "pairs")
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



# trace_df <- data.frame(data = "ncvr",
#                        trace = chain$overlap) %>%
#   mutate(iter = row_number())
#
# saveRDS(trace_df, "out/case_study_trace/ncvr")


