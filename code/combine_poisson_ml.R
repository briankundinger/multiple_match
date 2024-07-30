library(dplyr)

files <- list.files("out/poisson_ml/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

dupe_rate <- c("low", "mid", "high")

results$duplication <- dupe_rate[results$duplication]
results$method <- "multilink"

saveRDS(results, "out/poisson_ml_all")
