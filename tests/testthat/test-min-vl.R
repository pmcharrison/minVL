context("minVL")
suppressWarnings(library(reticulate))

test_that("ascending_distance", {
  expect_equal(
    get_vl_ascending_distance(60, 70, "pitch"),
    10
  )
  expect_equal(
    get_vl_ascending_distance(60, 60, "pitch"),
    0
  )
  expect_equal(
    get_vl_ascending_distance(1, 5, "pc"),
    4
  )
  expect_equal(
    get_vl_ascending_distance(7, 10, "pc"),
    3
  )
  expect_equal(
    get_vl_ascending_distance(11, 0, "pc"),
    1
  )
  expect_equal(
    get_vl_ascending_distance(7, 1, "pc"),
    6
  )
  expect_equal(
    get_vl_ascending_distance(6, 6, "pc"),
    0
  )
})

test_that("elt_distance", {
  expect_equal(
    get_vl_elt_distance(
      60, 64, "pitch"
    ),
    4
  )
  expect_equal(
    get_vl_elt_distance(
      50, 47, "pitch"
    ),
    3
  )
  expect_equal(
    get_vl_elt_distance(
      10, 7, "pc"
    ),
    3
  )
  expect_equal(
    get_vl_elt_distance(
      11, 0, "pc"
    ),
    1
  )
  expect_equal(
    get_vl_elt_distance(
      7, 1, "pc"
    ),
    6
  )
})

test_that("vl_dist", {
  # 1
  expect_equal(
    vl_dist(
      c(4, 7, 11), c(4, 4, 4),
      "pc", "taxicab"
    ),
    8
  )
  # 2
  expect_equal(
    vl_dist(
      c(4, 7, 7, 7), c(4, 8, 11, 3),
      "pc", "taxicab"
    ),
    9
  )
  # 3
  expect_equal(
    vl_dist(
      c(4, 7, 11, 0), c(4, 8, 11, 11),
      "pc", "taxicab"
    ),
    2
  )
  # 4
  expect_equal(
    vl_dist(
      c(60, 64, 67), c(60, 63, 67),
      "pitch", "taxicab"
    ),
    1
  )
  # 5
  expect_equal(
    vl_dist(
      c(58, 64, 67), c(60, 63, 67),
      "pitch", "taxicab"
    ),
    3
  )
  # 6
  expect_equal(
    vl_dist(
      c(60, 64, 67), c(60, 63, 67),
      "pitch", "euclidean"
    ),
    1
  )
  # 7
  expect_equal(
    vl_dist(
      c(58, 64, 67), c(55, 68, 67),
      "pitch", "euclidean"
    ),
    5
  )
  # 8
  expect_equal(
    vl_dist(
      c(11, 0, 4), c(2, 0, 8),
      "pc", "euclidean"
    ),
    5
  )
  # 9
  expect_equal(
    vl_dist(
      c(37, 38, 45), c(47, 40, 42),
      "pitch", "infinity"
    ),
    10
  )
  # 10
  expect_equal(
    vl_dist(
      c(11, 0, 4), c(5, 0, 8),
      "pc", "infinity"
    ),
    6
  )
})

test_that("order_by", {
  expect_equal(
    order_by(
      1:5, 5:1
    ),
    5:1
  )
  expect_equal(
    order_by(
      11:15, 5:1
    ),
    15:11
  )
  expect_equal(
    order_by(
      1:4, c(3, 2, -1, 4)
    ),
    c(3, 2, 1, 4)
  )
})

test_that("min_vl", {
  # 1
  expect_equal(
    min_vl(c(0, 4, 7), c(0, 3, 7),
           "pc", "taxicab")$dist,
    1
  )
  # 2
  expect_equal(
    min_vl(c(0, 4, 7), c(0, 5, 9),
           "pc", "taxicab")$dist,
    3
  )
  # 3
  expect_equal(
    min_vl(c(0, 4, 7), c(0, 3, 8),
           "pc", "euclidean")$dist,
    sqrt(2)
  )
  # 4
  expect_equal(
    min_vl(c(0, 4, 7), c(0, 3, 9),
           "pc", "infinity")$dist,
    2
  )
  # 5
  expect_equal(
    min_vl(c(50, 60, 65), c(48, 62, 67),
           "pitch", "taxicab")$dist,
    6
  )
  # 6
  expect_equal(
    min_vl(c(40, 60, 65), c(50, 60),
           "pitch", "taxicab")$dist,
    15
  )
  # 7
  expect_equal(
    min_vl(c(4, 7, 11, 0), c(4, 8, 11, 3),
           "pc", "taxicab")$dist,
    3
  )
  # 8
  expect_equal(
    min_vl(c(0, 4, 7), c(1, 6, 10),
           "pc", "taxicab")$dist,
    6
  )
  # 9
  expect_equal(
    min_vl(c(0, 4, 7, 10), c(1, 3, 7, 10),
           "pc", "taxicab")$dist,
    2
  )
  # Cache
  # expect_equal(
  #   min_vl(c(0, 4, 7, 10), c(1, 3, 7, 10),
  #                             "pc", "taxicab",
  #                             cache = FALSE),
  #   min_vl(c(0, 4, 7, 10), c(1, 3, 7, 10),
  #                             "pc", "taxicab",
  #                             cache = TRUE)
  # )
})

test_that("compare_with_python_implementation", {
  # Load Tymoczko's Python script
  py_run_file("min-vl.py")
  # Define a function that uses the Python implementation
  # to compute the minimal voice-leading distance
  # between two pitch-class sets
  py_min_vl <- function(s1, s2) {
    cmd <- sprintf(
      "res = nonbijective_vl([%s], [%s])",
      paste(s1, collapse = ", "),
      paste(s2, collapse = ", ")
    )
    res <- py_run_string(cmd)$res
    dist <- res[[1]]
    progression <- res[[2]]
    list(
      dist = dist,
      start = sapply(progression, function(x) x[1]),
      end = sapply(progression, function(x) x[2])
    )
  }

  # For some combinations of chords Tymoczko's implementation gets stuck
  # in an infinite loop, e.g. py_min_vl(c(1, 7, 5, 9, 0), c(7, 10)).
  # I have yet to diagnose this problem, but for the combination of seed and n
  # given below we don't run into any of these cases.
  set.seed(1)
  n <- 50
  elt_type <- "pc"
  norm <- "taxicab"
  # pb <- txtProgressBar(max = n, style = 3)
  for (i in 1:n) {
    chord_1 <- sample(0:11, size = sample(12, 1), replace = FALSE)
    chord_2 <- sample(0:11, size = sample(12, 1), replace = FALSE)
    our_implementation <- min_vl(chord_1, chord_2,
                                 elt_type = elt_type, norm = norm)
    expect_true(all(chord_1 %in% our_implementation$start))
    expect_true(all(chord_2 %in% our_implementation$end))
    expect_true(all(our_implementation$start %in% chord_1))
    expect_true(all(our_implementation$end %in% chord_2))
    expect_equal(vl_dist(
      our_implementation$start, our_implementation$end,
      elt_type = elt_type, norm = norm
    ), our_implementation$dist)
    tymoc_implementation <- py_min_vl(chord_1, chord_2)
    if (our_implementation$dist != tymoc_implementation$dist) {
      stop("Failure for chord_1 = c(", paste(chord_1, collapse = ", "),
           "); chord_2 = c(", paste(chord_2, collapse = ", "), ")")
    }
    expect_equal(our_implementation$dist, tymoc_implementation$dist)
    # setTxtProgressBar(pb, i)
  }
})

test_that("add_dist", {
  n <- 5
  elt_type <- "pc"
  for (norm in c("taxicab", "euclidean", "infinity")) {
    for (i in seq_len(20)) {
      norm <- "taxicab"
      s1a <- sample(x = 12, size = n)
      s2a <- sample(x = 12, size = n)
      s1 <- s1a[seq_len(n - 1)]
      s2 <- s2a[seq_len(n - 1)]
      elt_dist <- get_vl_elt_distance(s1a[n], s2a[n], elt_type = elt_type)
      expect_equal(
        vl_dist(s1a, s2a, elt_type = elt_type, norm = norm),
        add_dist(
          prev_dist = vl_dist(s1, s2, elt_type = elt_type, norm = norm),
          new_pair_dist = elt_dist,
          norm = norm
        )
      )
    }
  }
})
