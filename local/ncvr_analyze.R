library(tidyverse)

methods <- list.files("out/ncvr_results/eval/")
eval_files <- list.files("out/ncvr_results/eval/", full.names = T)
eval <- lapply(eval_files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  mutate(method = methods)


chain_mm <- readRDS("out/ncvr_results/chain/fabl_mm_2")
m <- rowMeans(chain_mm$m)
u <- rowMeans(chain_mm$u)
m <- rowMeans(chain_fabl$m)
u <- rowMeans(chain_fabl$u)
rowMeans(chain_mm$m) / rowMeans(chain_mm$u)
chain_fabl <- readRDS("out/ncvr_results/chain/fabl")
rowMeans(chain_fabl$m) / rowMeans(chain_fabl$u)


m_p <- sweep(hash$ohe, 2, log(m), "*") %>%
  rowSums() %>%
  exp()

u_p <- sweep(hash$ohe, 2, log(u), "*") %>%
  rowSums() %>%
  exp()

w_p <- m_p / u_p

E_m <- sum(w_p * m_p)
E_m

chain_mm$pi %>%
  do.call(rbind, .) %>%
  colMeans()
