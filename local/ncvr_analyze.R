library(tidyverse)

methods <- list.files("out/ncvr_results/eval/")
eval_files <- list.files("out/ncvr_results/eval/", full.names = T)
eval <- lapply(eval_files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  mutate(method = methods)
