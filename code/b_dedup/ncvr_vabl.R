library(vabldev)

# ncvr_a <- readRDS("data/ncvr_a")
# ncvr_b <- readRDS("data/ncvr_b_dedup")
S <- 1000
burn <- ceiling(S * .1)
#
#
# df1 <- ncvr_a %>%
#   select(voter_id) %>%
#   mutate(rn = row_number())
# # %>%
# #   arrange(voter_id)
#
# df2 <- ncvr_b %>%
#   select(voter_id) %>%
#   mutate(rn = row_number())
# #%>% arrange(voter_id)
#
# n1 <- nrow(df1)
# n2 <- nrow(df2)
#
# joined <- right_join(df1, df2, by = "voter_id", copy = T, keep = T,
#                      relationship = "many-to-many") %>%
#   arrange(voter_id.y)
#
# Z_true_pairs <- joined %>%
#   filter(!is.na(voter_id.x)) %>%
#   select(rn.x, rn.y)
# joined$rn[is.na(joined$rn)] <- 0
# Z_true <- joined$rn

Z_true_pairs <- readRDS("data/ncvr_Z_true_b_dedup")

hash <- readRDS("../../../../../usr/xtmp/bak47/mm/ncvr_b_dedupe/combine/hash")

n1 <- hash$n1
n2 <- hash$n2

ptm <- proc.time()
print(1)
chain <- vabl(hash)
saveRDS(chain, "../../../../../usr/xtmp/bak47/mm/ncvr_b_dedupe/chain/vabl")
seconds <- proc.time() - ptm
print(2)
results <- estimate_links(chain, hash, resolve = T)
Z_hat <- make_Zhat_pairs(results$Z_hat)
print(3)
saveRDS(Z_hat, "../../../../../usr/xtmp/bak47/mm/ncvr_b_dedupe/Z_hat/vabl")
saveRDS(results$prob[Z_hat[, 2]], "../../../../../usr/xtmp/bak47/mm/ncvr_b_dedupe/prob/vabl")
#Z_hat <- make_Zhat_pairs(results$Z_hat)

eval <- evaluate_links(Z_hat, Z_true_pairs, n1, "pairs")
print(4)
df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = S,
                 time = seconds[3],
                 method = "vabl",
                 data = "ncvr")
saveRDS(df, "../../../../../usr/xtmp/bak47/mm/ncvr_b_dedupe/eval/vabl")



# trace_df <- data.frame(data = "ncvr",
#                        trace = chain$overlap) %>%
#   mutate(iter = row_number())
#
# saveRDS(trace_df, "out/case_study_trace/ncvr")


