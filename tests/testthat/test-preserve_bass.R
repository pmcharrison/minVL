context("test-preserve_bass")

test_that("examples", {
  expect_equal(
    min_vl(c(0, 4, 7),
           c(7, 2, 11),
           preserve_bass = FALSE),
    list(dist = 3, start = c(0, 4, 7), end = c(11, 2, 7))
  )
  expect_equal(
    min_vl(c(0, 4, 7),
           c(7, 2, 11),
           preserve_bass = TRUE),
    list(dist = 8, start = c(0, 0, 4, 7), end = c(7, 11, 2, 7))
  )

  for (i in 1:100) {
    chords <- lapply(1:2, function(...) sample(0:11, size = 5, replace = FALSE))
    vl <- min_vl(chords[[1]], chords[[2]], preserve_bass = TRUE)
    expect_equal(vl$start[1], chords[[1]][1])
    expect_equal(vl$end[1], chords[[2]][1])
  }
})
