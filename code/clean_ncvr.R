library(dplyr)

ncvr <- read.csv("data/ncvr.csv")

ncvr_a <- ncvr %>%
  filter(file_id == "a") %>%
  #filter(!duplicated(voter_id)) %>%
  arrange(voter_id)

ncvr_b <- ncvr %>%
  filter(file_id == "b") %>%
  #filter(!duplicated(voter_id)) %>%
  arrange(voter_id)

saveRDS(ncvr_a, "data/ncvr_a")
saveRDS(ncvr_b, "data/ncvr_b")

ncvr_a <- ncvr %>%
  filter(file_id == "a") %>%
  #filter(!duplicated(voter_id)) %>%
  arrange(voter_id) %>%
  mutate(rn = row_number())

ncvr_b <- ncvr %>%
  filter(file_id == "b") %>%
  filter(!duplicated(voter_id)) %>%
  arrange(voter_id) %>%
  mutate(rn = row_number())


joined <- right_join(ncvr_a, ncvr_b, by = "voter_id", copy = T, keep = T,
                     relationship = "many-to-many") %>%
  arrange(voter_id.y)

Z_true_pairs <- joined %>%
  filter(!is.na(voter_id.x)) %>%
  select(rn.x, rn.y)

names(Z_true_pairs) <- c("target_id", "base_id")

#saveRDS(ncvr_a, "data/ncvr_a_dedup")
saveRDS(ncvr_b, "data/ncvr_b_dedup")
saveRDS(Z_true_pairs, "data/ncvr_Z_true_b_dedup")
