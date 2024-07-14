library(dplyr)

files <- list.files("out/sadinle_sim_ml_12_default/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

saveRDS(results, "out/sadinle_ml_all_12_default")
