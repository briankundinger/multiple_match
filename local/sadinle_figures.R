library(tidyverse)

results <- readRDS("out/sadinle_all")
df <- results %>%
  filter(method %in% c("fabl_mm", "fastlink", "multilink_2")) %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  mutate(metric = factor(metric,
                         c("recall", "precision", "f-measure"))) %>%
  group_by(errors, method, metric) %>%
  summarize(median = quantile(value, .5),
            lower = quantile(value, .025),
            upper = quantile(value, .975))

df %>%
  filter(metric != "f-measure") %>%
  ggplot() +
  aes(x = metric,
      y = median,
      min = lower,
      max = upper,
      color = method) +
  geom_pointrange(position = position_dodge2(width = .5)) +
  facet_wrap(~errors) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw()
