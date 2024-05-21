library(dplyr)

files <- list.files("out/ncvr/mms/mms_batch/", full.names = T)
mms <- lapply(files, readRDS) %>%
  purrr::flatten(.)

files <- list.files("out/ncvr/mms/prob_batch/", full.names = T)
prob <- lapply(files, readRDS) %>%
  do.call(c, .)

saveRDS(mms, paste0("out/ncvr/mms/combine/mms"))

saveRDS(prob, paste0("out/ncvr/mms/combine/prob"))
