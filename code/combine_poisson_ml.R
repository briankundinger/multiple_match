library(dplyr)

files <- list.files("out/poisson_2_ml/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame()

dupe_rate <- c("low", "mid", "high")

results$duplication <- dupe_rate[results$V5]
results$method <- "multilink"

results <- results %>%
  select(-V5)

saveRDS(results, "out/poisson_2_ml_all")
