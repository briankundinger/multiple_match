library(dplyr)
library(knitr)
library(kableExtra)

methods <- list.files("out/ncvr_dedup/eval/")
Z_hat_files <- list.files("out/ncvr_dedup/Z_hat/", full.names = T)
prob_files <- list.files("out/ncvr_dedup/prob/", full.names = T)
Z_true_pairs <- readRDS("data/ncvr_Z_true_b_dedup")

#saveRDS(prob, "out/ncvr_dedup/prob/fabl")
#drl_Z_hat <- Z_hat[, 1:2]
#saveRDS(drl_Z_hat, "out/ncvr_dedup/Z_hat/fabl")

eval_files <- list.files("out/ncvr_dedup/eval/", full.names = T)
eval <- lapply(eval_files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  mutate(method = methods)

n1 <- eval$n1[1]

temp <- lapply(seq_along(methods), function(i){
  Z_hat <- readRDS(Z_hat_files[i])
  prob <- readRDS(prob_files[i])
  if(i == 2){
  Z_hat <- Z_hat[, 1:2]
  }
  data.frame(Z_hat, prob)
})

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

evals_df$method[evals_df$method == "fabl_mm_inf"] <- "DRL"
evals_df$method[evals_df$method == "fabl"] <- "vabl"

evals_df %>%
  filter(method %in% c("vabl", "DRL", "fastlink", "fastlink_jaro")) %>%
  mutate(method = factor(method, c("DRL", "vabl", "fastlink", "fastlink_jaro"))) %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  mutate(metric = factor(metric, c("Recall", "Precision", "Fmeasure"))) %>%
  ggplot() +
  aes(x = threshold, y = value, color = method) +
  geom_line() +
  facet_wrap(~metric) +
  labs(y = NULL, color = NULL, x = NULL) +
  theme_bw(base_size = 9) +
  scale_y_continuous()

ggsave("figures/ncvr_thresholds.png")




#############


Z_hat_pair <- readRDS(Z_hat_files[2])%>%
  data.frame() %>%
  select(target_id, base_id) %>%
  tidyr::unite("pair")


readRDS(Z_hat_files[2])%>%
  data.frame() %>%
  select(target_id, base_id) %>%
  filter(duplicated(base_id)) %>%
  select(base_id) %>%
  pull() %>%
  length()

Z_true_pair <- Z_true_pairs %>%
  data.frame() %>%
  tidyr::unite("pair")

multiple_match_true <- Z_true_pairs %>%
  filter(duplicated(base_id)) %>%
  select(base_id) %>%
  pull()

multiple_match_declared <- readRDS(Z_hat_files[2]) %>%
  data.frame() %>%
  select(target_id, base_id) %>%
  filter(duplicated(base_id)) %>%
  select(base_id) %>%
  pull()

length(multiple_match_true)

mm_hat <- readRDS(Z_hat_files[2])%>%
  data.frame() %>%
  select(target_id, base_id) %>%
  filter(base_id %in% multiple_match_declared) %>%
  tidyr::unite("pair")

mm_true <- Z_true_pairs %>%
  data.frame() %>%
  filter(base_id %in% multiple_match_true) %>%
  tidyr::unite("pair")

Z_true_pairs %>%
  data.frame() %>%
  filter(base_id %in% multiple_match) %>%
  group_by(base_id) %>%
  count() %>%
  group_by(n) %>%
  count()

readRDS(Z_hat_files[2])%>%
  data.frame() %>%
  select(target_id, base_id) %>%
  data.frame() %>%
  filter(base_id %in% multiple_match_true) %>%
  group_by(base_id) %>%
  count() %>%
  group_by(n) %>%
  count()

false_doubles <- setdiff(mm_hat, mm_true) %>%
  unlist()

missed_doubles <- setdiff(mm_true, mm_hat) %>%
  unlist()

mm_recall <- (nrow(mm_true) - length(missed_doubles)) / nrow(mm_true)
mm_precision <- (nrow(mm_hat) - length(false_doubles)) / nrow(mm_hat)





false_matches <- setdiff(Z_hat_pair, Z_true_pair) %>%
  unlist()
df <- sapply(false_matches, function(x){
  stringr::str_split(x, "_")
}) %>%
  lapply(., as.numeric) %>%
  do.call(rbind, .) %>%
  data.frame()
colnames(df) <- c("target_id", "base_id")

letters_a <- ncvr_a$middle_name[df$target_id] %>%
  sapply(., nchar)

initial_a <- letters_a == 1

letters_b <- ncvr_b$middle_name[df$base_id] %>%
  sapply(., nchar)

initial_b <- letters_b == 1

thing <- cbind(initial_a, initial_b) %>%
  rowSums()

sum(thing > 0) / length(thing)

one_initial <-
sum(one_initial)

evals_df

