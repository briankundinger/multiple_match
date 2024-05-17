library(tidyverse)

methods <- list.files("out/ncvr_results/eval/")
Z_hat_files <- list.files("out/ncvr_results/Z_hat/", full.names = T)
prob_files <- list.files("out/ncvr_results/prob/", full.names = T)
Z_true_pairs <- readRDS("data/ncvr_Z_true")

eval <- lapply(eval_files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  mutate(method = methods)

n1 <- eval$n1[1]

temp <- lapply(seq_along(methods), function(i){
  Z_hat <- readRDS(Z_hat_files[i])
  prob <- readRDS(prob_files[i])
  data.frame(Z_hat, prob)
})

df <- temp[[1]]
threshold_vec <- seq(.5, .95, .05)
threshold <- threshold_vec[1]
evals_df <- lapply(threshold_vec, function(threshold){
  evals <- lapply(temp, function(df){
    filtered <- df %>%
      filter(prob > threshold) %>%
      select(-prob)

    vabldev::evaluate_links(filtered, Z_true_pairs, n1, "pairs")
    }) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    mutate(method = methods,
           threshold = threshold)
}) %>%
  do.call(rbind, .)

evals_df %>%
  filter(method %in% c("fabl", "fabl_mm_2", "fastlink", "fastlink_jaro")) %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  mutate(metric = factor(metric, c("Recall", "Precision", "Fmeasure"))) %>%
  ggplot() +
  aes(x = threshold, y = value, color = method) +
  geom_line() +
  facet_wrap(~metric) +
  labs(y = NULL, color = NULL, x = "Threshold") +
  theme_bw(base_size = 12)

ggsave("figures/ncvr_thresholds.png")
