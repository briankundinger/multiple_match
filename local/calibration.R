library(dplyr)
library(ggplot2)
library(tidyr)

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")

dupes_a <- ncvr_a$voter_id[duplicated(ncvr_a$voter_id)]
dupes_b <- ncvr_b$voter_id[duplicated(ncvr_b$voter_id)]
both <- intersect(dupes_a, dupes_b)

methods <- list.files("out/ncvr_results/eval/")
Z_hat_files <- list.files("out/ncvr_results/Z_hat/", full.names = T)
prob_files <- list.files("out/ncvr_results/prob/", full.names = T)

temp <- lapply(seq_along(methods), function(i){
  Z_hat <- readRDS(Z_hat_files[i])
  names(Z_hat) <- c("target_id", "base_id")
  prob <- readRDS(prob_files[i])
  data.frame(Z_hat, prob) %>%
    filter(!is.na(prob)) %>%
    filter(prob > .5)
})

calibrations <- lapply(temp, function(Z_hat){
  Z_hat %>%
    mutate(true_match = ncvr_a$voter_id[target_id] == ncvr_b$voter_id[base_id]) %>%
    mutate(bin = cut(prob, breaks = seq(.5, 1, .05))) %>%
    group_by(bin, .drop = F) %>%
    summarize(percent_match = mean(true_match),
              n = n()) %>%
    mutate(marker = seq(.525, .975, .05))
})
names(calibrations) <- methods
for(i in seq_along(methods)){
df_calib <- calibrations[[i]]
df_calib %>%
  ggplot() +
  aes(x = marker, y = percent_match, size = n) %>%
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "Posterior Probability", y = "Percent True Matches",
       size = NULL, title = paste0(methods[i])) +
  scale_x_continuous(limits = c(.5, 1)) +
  scale_y_continuous(limits = c(0, 1))+
  theme_bw(base_size = 12)


ggsave(paste0("figures/calibration/", methods[i], ".png"))
}

# fl_out <- readRDS("out/ncvr_results/chain/fastlink")
# fl_out$posterior %>%
#   is.na() %>%
#   sum()

temp[[2]] %>%
  filter(prob > .5) %>%
  group_by(base_id) %>%
  count() %>%
  group_by(n) %>%
  count()

temp[[2]] %>%
  mutate(true_match = ncvr_a$voter_id[target_id] == ncvr_b$voter_id[base_id]) %>%
  filter(prob > .9) %>%
  group_by(base_id) %>%
  summarise(n_correct = sum(true_match), n_declared = n()) %>%
  mutate(percent_correct = n_correct / n_declared) %>%
  group_by(percent_correct, n_declared) %>%
  count()
