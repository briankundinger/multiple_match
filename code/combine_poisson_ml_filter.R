library(dplyr)

files <- list.files("out/poisson_ml_filter/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .) %>%
  mutate(method = "multilink_filter")

saveRDS(results, "out/poisson_ml_all_filter")
