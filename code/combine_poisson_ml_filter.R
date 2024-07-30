library(dplyr)

files <- list.files("out/poisson_ml_filter/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame()

dupe_rate <- c("low", "mid", "high")

results$duplication <- dupe_rate[results$V5]
results$method <- "multilink"

results %>%
  select(-V5)


saveRDS(results, "out/poisson_ml_filter_all") %>%
  as.data.frame()
