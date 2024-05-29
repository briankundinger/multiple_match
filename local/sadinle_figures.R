library(dplyr)
library(ggplot2)
library(tidyr)

results <- readRDS("out/sadinle_all")
results_ml <- readRDS("out/sadinle_ml_all_12") %>%
  data.frame() %>%
  mutate(method = "multilink") %>%
  relocate(method, .before = errors)
names(results_ml)[1:4] <- names(results)[1:4]
names(results_ml)
names(results)


results_all <- rbind(results, results_ml)


#results[is.na(results)] <- 0
df <- results_all %>%
  filter(method %in% c("fabl_mm_inf", "fastlink", "multilink")) %>%
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
  facet_grid(overlap~errors, scales = "free_y") +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw()

ggsave("figures/recall_precision_2_to_1.png")

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
