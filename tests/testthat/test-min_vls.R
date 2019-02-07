context("test-min_vls")

test_that("examples", {
  s1 <- list(c(0, 4, 7), c(2, 7, 9), c(3, 7, 8, 10))
  s2 <- c(1, 5, 10)

  expect_equal(
    min_vls(s1, s2),
    lapply(s1, function(x) min_vl(x, s2))
  )
})
