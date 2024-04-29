library(dplyr)

files <- list.files("out/sadinle_sim/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

saveRDS(results, "out/sadinle_all")
