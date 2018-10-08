context("test-min_vl_dists")

test_that("min_vl_dists", {
  dict <- lapply(1:10, function(i) sample(11, size = 3))
  n <- 100
  s1_list <- sample(dict, size = n, replace = TRUE)
  s2_list <- sample(dict, size = n, replace = TRUE)

  res_1 <- matrix(nrow = n, ncol = n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      res_1[i, j] <- min_vl_dist(s1_list[[i]], s2_list[[j]])
    }
  }
  res_2 <- min_vl_dists(s1_list, s2_list)
  expect_equal(res_1, res_2)
})
