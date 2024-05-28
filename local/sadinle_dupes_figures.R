library(dplyr)
library(ggplot2)
library(tidyr)

results <- readRDS("out/sadinle_double_dupes_all")
results_ml <- readRDS("out/sadinle_ml_all")
error_vec <- c("One Error", "Two Errors", "Three Errors")
results$errors <- error_vec[results$errors]
results$errors <- factor(results$errors, error_vec)

overlap_vec <- c("Low Overlap", "Mid Overlap", "High Overlap")
results$overlap <- match(results$overlap, unique(results$overlap))
results$overlap <- overlap_vec[results$overlap]
results$overlap <- factor(results$overlap, overlap_vec)
results[is.na(results)] <- 0
df <- results %>%
  filter(method %in% c("fabl_mm_inf", "fastlink")) %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  mutate(metric = factor(metric,
                         c("recall", "precision", "f-measure"))) %>%
  group_by(errors, method, overlap, metric) %>%
  summarize(median = quantile(value, .5, na.rm = T),
            lower = quantile(value, .025, na.rm = T),
            upper = quantile(value, .975, na.rm = T))

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
  facet_grid(overlap~errors) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw(base_size = 12)

ggsave("figures/sadinle_recall_precision_2to2.png")

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
