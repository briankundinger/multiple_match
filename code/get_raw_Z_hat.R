library(dplyr)
library(parallel)
library(vabldev)


out <- readRDS("out/ncvr_results/chain/fabl_mm_2")
hash <- readRDS("out/ncvr/combine/hash")
result <- estimate_links_mm(out, hash, resolve = F, transitivity = F)

saveRDS(result$Z_hat, "out/ncvr_results/Z_hat/fabl_mm_2_raw")
saveRDS(result$prob, "out/ncvr_results/prob/fabl_mm_2_raw")
