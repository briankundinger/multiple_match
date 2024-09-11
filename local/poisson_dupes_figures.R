library(dplyr)
library(ggplot2)
library(tidyr)

results <- readRDS("out/poisson_all")
results_ml <- readRDS("out/poisson_ml_all")
results_filter <- readRDS("out/poisson_ml_filter_all")
results <- rbind(results, results_ml, results_filter)

# results <- readRDS("out/poisson_2_all")
# results_ml <- readRDS("out/poisson_2_ml_all")
# results_filter <- readRDS("out/poisson_2_ml_filter_all")
# results <- rbind(results, results_ml, results_filter)

names(results)[3] <- "F-measure"

df <- results %>%
  pivot_longer(cols = 1:4, names_to = "metric") %>%
  mutate(metric = factor(metric,
                         c("Recall", "Precision", "F-measure", "elapsed"))) %>%
  mutate(duplication = factor(duplication,
                         c("low", "mid", "high"))) %>%
  mutate(method = factor(method,
                              c("vabl", "fastLink", "multilink",
                                "multilink_filter", "DRL"))) %>%
  group_by(method, metric, duplication) %>%
  summarize(median = quantile(value, .5, na.rm = T),
            lower = quantile(value, .025, na.rm = T),
            upper = quantile(value, .975, na.rm = T))



# df %>%
#   ggplot() +
#   aes(x = method, y = median, min = lower, max = upper) +
#   geom_pointrange(position = position_dodge2(width = .5),
#                   size = .3) +
#   facet_grid(metric ~ duplication, scales = "free") +
#   labs(x = NULL, y = NULL) +
#   theme_bw(base_size = 8)

df %>%
  filter(method %in% c("vabl", "DRL", "fastLink")) %>%
  filter(metric != "elapsed") %>%
  ggplot() +
  aes(x = method, y = median, min = lower, max = upper) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .2) +
  facet_grid(metric ~ duplication, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8)

ggsave("figures/poisson_fig1.png")
#ggsave("figures/poisson_fig1_app.png")


df %>%
  filter(method %in% c("multilink", "multilink_filter", "DRL")) %>%
  filter(metric != "elapsed") %>%
  ggplot() +
  aes(x = method, y = median, min = lower, max = upper) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .2) +
  facet_grid(metric ~ duplication, scales = "free") +
  labs(x = NULL, y = NULL) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw(base_size = 9)

ggsave("figures/poisson_fig2.png")
#ggsave("figures/poisson_fig2_app.png")

df %>%
  filter(method %in% c("fabl", "DRL")) %>%
  filter(metric == "elapsed") %>%
  ggplot() +
  aes(x = method, y = median, min = lower, max = upper) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .2) +
  facet_wrap(~duplication) +
  labs(x = NULL, y = "Seconds elapsed") +
  theme_bw(base_size = 9)

ggsave("figures/poisson_time.png")




