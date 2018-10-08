context("test-min_vl_dist")

test_that("examples", {
  n <- 20
  for (i in seq_len(n)) {
    x <- sample(11, size = 3)
    y <- sample(11, size = 4)
    expect_equal(min_vl_dist(x, y),
                 min_vl(x, y)$dist)
    expect_equal(min_vl_dist(x, y, elt_type = "pitch"),
                 min_vl(x, y, elt_type = "pitch")$dist)
    expect_equal(min_vl_dist(x, y, norm = "euclidean"),
                 min_vl(x, y, norm = "euclidean")$dist)
  }
})
