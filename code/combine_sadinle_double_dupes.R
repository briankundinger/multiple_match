library(dplyr)

files <- list.files("out/sadinle_double_dupes/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

saveRDS(results, "out/sadinle_double_dupes_all")
