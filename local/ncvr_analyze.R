library(dplyr)
library(knitr)
library(kableExtra)

methods <- list.files("out/ncvr_results/eval/")
eval_files <- list.files("out/ncvr_results/eval/", full.names = T)
eval <- lapply(eval_files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  mutate(method = methods)

flink <- eval %>%
  filter(method %in% c("fastlink", "fastlink_jaro")) %>%
  select(recall, precision, f_measure, method)

xtmp_eval <- list()

eval_files <- list.files("out/xtmp/", full.names = T)
Z_hats <- lapply(eval_files, readRDS)
xtmp_eval[[1]] <- vabldev::evaluate_links(Z_hats[[1]], Z_true_dedup, 250000, "pairs")
Z_hat_dedup <- Z_hats[[2]] %>%
  select(-prob)

Z_true_dedup <- readRDS("data/ncvr_Z_true_b_dedup")
xtmp_eval[[2]] <- vabldev::evaluate_links(Z_hat_dedup, Z_true_dedup, 250000, "pairs")

Z_hat_swap <- Z_hats[[3]]
Z_hat_swap <- Z_hat_swap[, c(2, 1)] %>%
  data.frame()
names(Z_hat_swap) <- c("target_id", "base_id")


xtmp_eval[[3]] <- vabldev::evaluate_links(Z_hat_swap, Z_true_dedup, 250000, "pairs")

xtmp <- do.call(rbind, xtmp_eval) %>%
  data.frame(., method = c("fabl", "fabl_mm", "fabl_swap"))

names(xtmp) <- names(flink)

df <- rbind(xtmp, flink) %>%
  relocate(method, .before = recall)
rownames(df) <- NULL

df %>%
  kable("latex")

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b_dedup")
Z_hat <- readRDS("out/ncvr_results/Z_hat/fabl_mm_2")
Z_true <- readRDS("data/ncvr_Z_true_b_dedup")

Z_hat_pair <- Z_hat %>%
  data.frame() %>%
  tidyr::unite("pair")

Z_true_pair <- Z_true %>%
  data.frame() %>%
  tidyr::unite("pair")

setdiff(Z_hat_pair[1:100, ], Z_true_pair[1:100])


letters_a <- ncvr_a$middle_name[Z_hat$target_id] %>%
  sapply(., nchar)

initial_a <- letters_a == 1

letters_b <- ncvr_b$middle_name[Z_hat$base_id] %>%
  sapply(., nchar)

initial_b <- letters_b == 1

one_initial <- initial_a | initial_b

