library(dplyr)

files <- list.files("out/poisson_fabl/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

saveRDS(results, "out/poisson_fabl_ all")

# files <- list.files("out/poisson_2/", full.names = T)
# results <- lapply(files, readRDS) %>%
#   do.call(rbind, .)
#
# saveRDS(results, "out/poisson_2_all")
