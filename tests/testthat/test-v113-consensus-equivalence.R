test_that("complete co-assignment matches the v1.1.2 implementation", {
  for (seed in seq_len(150L)) {
    set.seed(12000L + seed)
    n <- sample(8:100, 1L)
    k <- sample(2:min(8L, n), 1L)
    n_records <- sample(2:25, 1L)
    records <- lapply(seq_len(n_records), function(i) {
      labels <- as.integer(sample(rep(seq_len(k), length.out = n)))
      if (seed %% 3L == 0L) {
        names(labels) <- paste0("sample_", seq_len(n))
      }
      list(
        id = paste0("fit_", i),
        k = k,
        sample_labels = labels
      )
    })

    expected <- .v112_coassignment(records)
    observed <- SOMevidence:::.complete_coassignment(records, n)
    diag(observed) <- 1

    expect_identical(observed, expected, info = paste("seed", seed))
    expect_identical(typeof(observed), "double")
  }
})

test_that("incremental alignment matches v1.1.2 across fixed random cases", {
  for (seed in seq_len(150L)) {
    set.seed(13000L + seed)
    n <- sample(30:120, 1L)
    k <- sample(2:min(6L, n), 1L)
    n_records <- sample(3:14, 1L)
    truth <- sample(rep(seq_len(k), length.out = n))
    anchors <- vapply(
      seq_len(k), function(cluster) which(truth == cluster)[[1L]], integer(1)
    )
    records <- lapply(seq_len(n_records), function(i) {
      permutation <- sample(seq_len(k))
      labels <- permutation[truth]
      observed <- stats::runif(n) < stats::runif(1L, 0.35, 0.9)
      observed[anchors] <- TRUE
      noisy <- observed & !(seq_len(n) %in% anchors) &
        stats::runif(n) < 0.1
      labels[noisy] <- sample(seq_len(k), sum(noisy), replace = TRUE)
      labels[!observed] <- NA_integer_
      list(
        id = paste0("fit_", i),
        sample_labels = as.integer(labels)
      )
    })
    reference_index <- sample(seq_len(n_records), 1L)

    expected <- .v112_propagate_alignment(records, reference_index, k)
    observed <- SOMevidence:::.propagate_alignment(
      records, reference_index, k
    )

    expect_identical(observed, expected, info = paste("seed", seed))
  }
})

test_that("incremental alignment preserves ties, order, and errors", {
  records <- list(
    list(id = "reference", sample_labels = c(1L, 1L, 2L, 2L, NA, NA)),
    list(id = "first", sample_labels = c(2L, 2L, 1L, 1L, 1L, 2L)),
    list(id = "second", sample_labels = c(2L, 2L, 1L, 1L, 2L, 1L)),
    list(id = "third", sample_labels = c(1L, 2L, 1L, 2L, 1L, 2L))
  )
  expect_identical(
    SOMevidence:::.propagate_alignment(records, 1L, 2L),
    .v112_propagate_alignment(records, 1L, 2L)
  )

  unresolved <- list(
    list(id = "reference", sample_labels = c(1L, 1L, NA, NA)),
    list(id = "child", sample_labels = c(1L, 1L, 2L, 2L))
  )
  expected_message <- paste0(
    "Consensus labels could not be propagated through the partition-",
    "overlap graph because an intermediate mapping was unidentifiable."
  )
  expect_error(
    .v112_propagate_alignment(unresolved, 1L, 2L),
    expected_message,
    fixed = TRUE
  )
  expect_error(
    SOMevidence:::.propagate_alignment(unresolved, 1L, 2L),
    expected_message,
    fixed = TRUE
  )

  single <- list(list(
    id = "only",
    sample_labels = c(1L, 1L, 2L, 2L)
  ))
  expect_identical(
    SOMevidence:::.propagate_alignment(single, 1L, 2L),
    .v112_propagate_alignment(single, 1L, 2L)
  )
})

test_that("incremental alignment preserves dynamic unlocking diagnostics", {
  records <- list(
    list(
      id = "reference",
      sample_labels = c(1L, 1L, 1L, 1L, 2L, 2L, NA, NA)
    ),
    list(
      id = "invalid",
      sample_labels = c(1L, 2L, 1L, 2L, NA, NA, 1L, 2L)
    ),
    list(
      id = "valid",
      sample_labels = c(1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L)
    )
  )
  propagated <- SOMevidence:::.propagate_alignment(records, 1L, 2L)

  expect_identical(propagated$diagnostics$child_fit, c("valid", "invalid"))
  expect_identical(
    propagated$diagnostics$parent_fit,
    c("reference", "reference;valid")
  )
  expect_identical(propagated$diagnostics$n_joint, c(6L, 10L))
  expect_identical(propagated, .v112_propagate_alignment(records, 1L, 2L))
})

test_that("consensus dispatch and incomplete-label slow path are unchanged", {
  records <- list(
    list(
      id = "complete", k = 2L,
      sample_labels = c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L)
    ),
    list(
      id = "partial_a", k = 2L,
      sample_labels = c(2L, 2L, NA, 1L, 1L, NA, 2L, 1L)
    ),
    list(
      id = "partial_b", k = 2L,
      sample_labels = c(NA, 1L, 1L, NA, 2L, 2L, 1L, 2L)
    )
  )
  partitions <- structure(
    list(
      records = records,
      scope = "analysis",
      ensemble = list(data = list(metadata = data.frame(
        id = paste0("sample_", seq_len(8L))
      )))
    ),
    class = "som_partitions"
  )

  explicit <- consensus_som(partitions, k = 2L, method = "coassignment")
  automatic <- consensus_som(partitions, k = 2L, method = "auto")

  expect_identical(explicit$coassignment, .v112_coassignment(records))
  expect_identical(explicit$method, "coassignment")
  expect_identical(automatic$method, "aligned_vote")

  disjoint <- partitions
  disjoint$records <- list(
    list(
      id = "left", k = 2L,
      sample_labels = c(1L, 2L, 1L, 2L, rep(NA_integer_, 4L))
    ),
    list(
      id = "right", k = 2L,
      sample_labels = c(rep(NA_integer_, 4L), 1L, 2L, 1L, 2L)
    )
  )
  expect_error(
    consensus_som(disjoint, k = 2L, method = "coassignment"),
    "Some sample pairs were never jointly assigned across the ensemble.",
    fixed = TRUE
  )
})

test_that("co-assignment size boundaries preserve automatic dispatch", {
  n <- 10L
  records <- list(
    list(
      id = "first", k = 2L,
      sample_labels = rep(1:2, each = n / 2L)
    ),
    list(
      id = "second", k = 2L,
      sample_labels = rep(2:1, each = n / 2L)
    )
  )
  partitions <- structure(
    list(
      records = records,
      scope = "analysis",
      ensemble = list(data = list(metadata = data.frame(
        id = paste0("sample_", seq_len(n))
      )))
    ),
    class = "som_partitions"
  )

  at_limit <- consensus_som(
    partitions,
    k = 2L,
    method = "auto",
    max_coassignment_n = n
  )
  above_limit <- consensus_som(
    partitions,
    k = 2L,
    method = "auto",
    max_coassignment_n = n - 1L
  )
  expect_identical(at_limit$method, "coassignment")
  expect_identical(above_limit$method, "aligned_vote")
  expect_error(
    consensus_som(
      partitions,
      k = 2L,
      method = "coassignment",
      max_coassignment_n = n - 1L
    ),
    "exceeds `max_coassignment_n`",
    fixed = TRUE
  )
})
