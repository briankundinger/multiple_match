library(dplyr)

files <- list.files("out/poisson_ml/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .) %>%
  mutate(method = "multilink")

saveRDS(results, "out/poisson_ml_all")
