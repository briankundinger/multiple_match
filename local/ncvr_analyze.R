library(dplyr)

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

eval_files <- list.files("out/xtmp/", full.names = T)
Z_hats <- lapply(eval_files, readRDS)
Z_hat_dedup <- Z_hats[[1]] %>%
  select(-prob)

Z_true_dedup <- readRDS("data/ncvr_Z_true_b_dedup")
vabldev::evaluate_links(Z_hat_dedup, Z_true_dedup, 250000, "pairs")

Z_hat_swap <- Z_hats[[2]]
Z_hat_swap <- Z_hat_swap[, c(2, 1)] %>%
  data.frame()
names(Z_hat_swap) <- c("target_id", "base_id")
Z_hat <- Z_hat_swap

Z_true <- readRDS("data/ncvr_Z_true")
Z_true <- Z_true %>%
  filter(!duplicated(rn.y))

vabldev::evaluate_links(Z_hat, Z_true_dedup, 250000, "pairs")





n_links <- dim(Z_hat)[1]
n_matches <- dim(Z_true)[1]
Z_hat_pair <- Z_hat %>%
  data.frame() %>%
  tidyr::unite("pair")

Z_true_pair <- Z_true %>%
  data.frame() %>%
  tidyr::unite("pair")

n_correct_links <- intersect(Z_hat_pair$pair, Z_true_pair$pair) %>%
  #pull() %>%
  length()
