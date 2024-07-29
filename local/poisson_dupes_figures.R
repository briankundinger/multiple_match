library(dplyr)
library(ggplot2)
library(tidyr)

results <- readRDS("out/poisson_all")


#results[is.na(results)] <- 0
df <- results %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  mutate(metric = factor(metric,
                         c("Recall", "Precision", "Fmeasure"))) %>%
  mutate(duplication = factor(duplication,
                         c("low", "mid", "high"))) %>%
  mutate(method = factor(method,
                              c("vabl", "fastLink", "DRL"))) %>%
  group_by(method, metric, duplication) %>%
  summarize(median = quantile(value, .5, na.rm = T),
            lower = quantile(value, .025, na.rm = T),
            upper = quantile(value, .975, na.rm = T))

df %>%
  ggplot() +
  aes(x = method, y = median, min = lower, max = upper) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .4) +
  facet_grid(metric ~ duplication, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 9)

ggsave("figures/poisson_sims.png")

df %>%
  filter(metric == "recall") %>%
  ggplot() +
  aes(x = method,
      y = median,
      min = lower,
      max = upper,
      color = method) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .4) +
  facet_grid(overlap~errors, scales = "free_y") +
  labs(x = NULL, y = "Recall", color = NULL) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_bw(base_size = 9)

ggsave("figures/sadinle_recall.png")

df %>%
  filter(metric == "precision") %>%
  ggplot() +
  aes(x = method,
      y = median,
      min = lower,
      max = upper,
      color = method) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .4) +
  facet_grid(overlap~errors, scales = "free_y") +
  labs(x = NULL, y = "Precision", color = NULL) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_bw(base_size = 9)

ggsave("figures/sadinle_precision.png")


df %>%
  filter(metric == "f-measure") %>%
  ggplot() +
  aes(x = method,
      y = median,
      min = lower,
      max = upper,
      color = method) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .4) +
  facet_grid(overlap~errors, scales = "free_y") +
  labs(x = NULL, y = "F-Measure", color = NULL) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_bw(base_size = 9)

ggsave("figures/sadinle_fmeasure.png")



# results %>%
#   filter(method %in% c("fabl_mm_inf", "fastlink")) %>%
#   pivot_longer(cols = 1:3, names_to = "metric") %>%
#   mutate(metric = factor(metric,
#                          c("recall", "precision", "f-measure"))) %>%
#   group_by(errors, method, overlap, metric) %>%
#   summarize(fails = sum(value == 0, na.rm = T))

df %>%
  filter(metric != "f-measure") %>%
  ggplot() +
  aes(x = metric,
      y = median,
      min = lower,
      max = upper,
      color = method) +
  geom_pointrange(position = position_dodge2(width = .5)) +
  facet_grid(overlap~errors, scales = "free_y") +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw()

#ggsave("figures/recall_precision_2_to_1.png")

# df %>%
#   filter(metric == "f-measure") %>%
#   ggplot() +
#   aes(x = metric,
#       y = median,
#       min = lower,
#       max = upper,
#       color = method) +
#   geom_pointrange(position = position_dodge2(width = .5)) +
#   facet_grid(overlap~errors) +
#   labs(x = NULL, y = NULL, color = NULL) +
#   theme_bw()
