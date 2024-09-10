library(dplyr)
library(ggplot2)

chain <- readRDS("out/poisson_sim_chain")

eta_samps <- lapply(1:900, function(x){
  chain$eta[[x]] %>%
    data.frame() %>%
    mutate(iter = x) %>%
    mutate(k = row_number())
}) %>%
  do.call(rbind, .)

names(eta_samps)[1] <- "value"

eta_samps %>%
  ggplot() +
  aes(x = iter, y = value) +
  geom_line() +
  facet_wrap(~k, ncol = 3) +
  labs(x = "Iteration", y = expression(eta[k])) +
  theme_bw(base_size = 9)

ggsave("figures/poisson_eta_trace.png")




