library(vabldev)

out <- readRDS("out/ncvr_results/chain/fabl_mm_2")
hash <- readRDS("out/ncvr/combine/hash")
Z_true_pairs <- readRDS("data/ncvr_Z_true")

result <- get_mms(out, hash)
saveRDS(result$Z_hat, "out/ncvr_results/eval/fabl_mm_2_mms")
