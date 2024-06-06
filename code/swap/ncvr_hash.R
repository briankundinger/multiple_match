library(vabldev)
library(glue)
library(tictoc)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
ncvr_a <- readRDS("data/ncvr_a")
ncvr_a[ncvr_a == ""] <- NA
ncvr_b <- readRDS("data/ncvr_b_dedup")
ncvr_b[ncvr_b == ""] <- NA

n1 <- nrow(ncvr_b)
n2 <- nrow(ncvr_a)

batch_size <- 100

normal_batches <- n2 %/% batch_size
last_batch <- n2 %% batch_size

batch_id <-c(rep(1:normal_batches, each = batch_size), rep(normal_batches + 1, last_batch))
batch <- ncvr_a[batch_id == k, ]

#fields <- c(4, 5, 6, 7, 9, 10, 13)
#types <- rep("bi", length(fields))

fields <- c(4, 5, 6, 7, 9, 10)
types <- c("lv", "bi", "lv", "bi", "bi", "bi")
#2241

# fields <- c(4, 5, 6)
# types <- c("lv", "bi", "lv")
start <- tic()
cd <- compare_records(ncvr_b, batch, fields = fields, types = types,
                      breaks = c(0, .25))
compare_time <- unname(toc(quiet = T)$toc - start)
start <- tic()
hash <- hash_comparisons(cd, all_patterns = T, R = 10, algorithm = c("vabl", "fabl", "FS"))

hash_time <- unname(toc(quiet = T)$toc - start)

# time_df <- data.frame(batch = k,
#                       data = "NCVR",
#                       comparison = compare_time,
#                       hash = hash_time)
# saveRDS(time_df, glue("out/case_study_time/ncvr_{k}"))
# paste0("out/ncvr/hash/", "hash_",
#        stringr::str_pad(k, 2, pad = "0"))
saveRDS(hash, paste0("../../../../../usr/xtmp/bak47/mm/swap/hash/",
                     "hash_",
                     stringr::str_pad(k, 4, pad = "0")))
