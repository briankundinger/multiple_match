library(vabldev)

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")


n1 <- nrow(ncvr_a)
n2 <- nrow(ncvr_b)

joined <- right_join(df1, df2, by = "voter_id", copy = T, keep = T,
                     relationship = "many-to-many") %>%
  arrange(voter_id.y)

Z_true_pairs <- joined %>%
  filter(!is.na(voter_id.x)) %>%
  select(rn.x, rn.y)

ptm <- proc.time()
fl_out <- fastLink::fastLink(ncvr_a, ncvr_b, varnames = names(ncvr_a)[c(4, 5, 6, 7, 9, 10)],
                   stringdist.match = names(ncvr_a)[c(4, 6)],
                   partial.match = names(ncvr_a)[c(4, 6)],
                   cut.a = 1, cut.p = .75, dedupe.matches = F, threshold.match = .5)
seconds <- proc.time() - ptm

Z_hat <- data.frame(id_1 = fl_out$matches$inds.a,
                    id_2 = fl_out$matches$inds.b)

eval <- evaluate_links(Z_hat, Z_true_pairs, n1, "pairs")
df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = tmax,
                 time = seconds[3],
                 method = "fastlink",
                 data = "ncvr")
saveRDS(df, "out/ncvr_results/fastlink")



