library(dplyr)
library(ggplot2)

chain <- readRDS("out/ncvr_dedup/fabl_mm_inf")
# eta

eta_samps <- lapply(1:900, function(x){
  chain$pi[[x]] %>%
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

# m


fields <- names(file_A)[c(4, 5, 6, 7, 8)]
levels <- c(3, 3, 2, 2, 2)
fields_vec <- sapply(1:5, function(x){
  rep(fields[x], levels[x])
}) %>%
  unlist()

k_vec <- sapply(1:5, function(x){
  seq_len(levels[x])
}) %>%
  unlist()
values <- as.vector(chain$m)

m_df <- data.frame(value = values, field = rep(fields_vec, 900), k = rep(k_vec, 900), iter = rep(1:900, each = 12))


m_df %>%
  ggplot() +
  aes(x = iter, y = value) +
  geom_line() +
  facet_grid(k ~field) +
  labs(x = "Iteration", y = expression(m[fl])) +
  theme_bw(base_size = 9)

ggsave("figures/poisson_m_trace.png")


values <- as.vector(chain$u)

u_df <- data.frame(value = values, field = rep(fields_vec, 900), k = rep(k_vec, 900), iter = rep(1:900, each = 12))

u_df %>%
  ggplot() +
  aes(x = iter, y = value) +
  geom_line() +
  facet_grid(k ~field) +
  labs(x = "Iteration", y = expression(u[fl])) +
  theme_bw(base_size = 9)

ggsave("figures/poisson_u_trace.png")

number_of_links <- apply(chain$Z, MARGIN = c(1, 2), FUN = function(x){sum(!is.na(x))})
total_matches <- colSums(number_of_links)
df <- data.frame(value = total_matches, iter = 1:900)

df %>%
  ggplot() +
  aes(x = iter, y = value) +
  geom_line() +
  labs(x = "Iteration", y = NULL) +
  theme_bw(base_size = 9)

ggsave("figures/poisson_total_match_trace.png")
