library(dplyr)
library(ggplot2)
library(tidyr)

results <- readRDS("out/sadinle_all")
results_ml <- readRDS("out/sadinle_ml_all_12") %>%
  data.frame() %>%
  mutate(method = "multilink") %>%
  relocate(method, .before = errors)
names(results_ml)[1:4] <- names(results)[1:4]
results_all <- rbind(results, results_ml)

error_vec <- c("One Error", "Two Errors", "Three Errors")
results_all$errors <- error_vec[results_all$errors]
results_all$errors <- factor(results_all$errors, error_vec)

overlap_vec <- c("Low Overlap", "Mid Overlap", "High Overlap")
results_all$overlap <- match(results_all$overlap, unique(results_all$overlap))
results_all$overlap <- overlap_vec[results_all$overlap]
results_all$overlap <- factor(results_all$overlap, overlap_vec)


#results[is.na(results)] <- 0
df <- results_all %>%
  filter(method %in% c("fabl_mm_inf", "fastlink", "multilink", "fabl_swap")) %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  mutate(metric = factor(metric,
                         c("recall", "precision", "f-measure"))) %>%
  group_by(errors, method, overlap, metric) %>%
  summarize(median = quantile(value, .5, na.rm = T),
            lower = quantile(value, .025, na.rm = T),
            upper = quantile(value, .975, na.rm = T))

df$method[df$method == "fabl_mm_inf"] <- "fabl_mm"

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
