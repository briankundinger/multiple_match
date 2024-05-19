library(vabldev)

out <- readRDS("out/ncvr_results/chain/fabl_mm_inf")
hash <- readRDS("out/ncvr/combine/hash")
Z_true_pairs <- readRDS("data/ncvr_Z_true")

result <- get_mms(out, hash)
saveRDS(result$Z_hat, "out/ncvr_results/Z_hat/fabl_mm_inf_mms")
saveRDS(result$prob, "out/ncvr_results/prob/fabl_mm_inf_mms")

eval <- evaluate_links(result$Z_hat, Z_true_pairs, hash$n1, "pairs")

df <- data.frame(n1 = hash$n1,
                 n2 = hash$n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = 1000,
                 time = NA,
                 method = "fabl_mm_inf_mms",
                 data = "ncvr")
saveRDS(df, "out/ncvr_results/eval/fabl_mm_inf_mms")
