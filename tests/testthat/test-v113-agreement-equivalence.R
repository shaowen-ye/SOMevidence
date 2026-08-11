test_that("agreement metrics preserve the version 1.1.2 kernels", {
  cases <- list(
    no_joint = list(c(1L, NA), c(NA, 1L)),
    one_joint = list(c(1L, NA), c(7L, 2L)),
    constant = list(rep(1L, 20L), rep(9L, 20L)),
    crossed = list(rep(1:2, each = 10L), rep(2:1, times = 10L)),
    non_contiguous = list(c(2L, 2L, 9L, 9L), c(8L, 8L, 3L, 3L)),
    unused_factors = list(
      factor(c("a", "a", "b", NA), levels = c("a", "b", "unused")),
      factor(c("x", "x", "y", "y"), levels = c("x", "y", "unused"))
    )
  )
  set.seed(11340)
  for (i in seq_len(50L)) {
    n <- sample(2:80, 1L)
    cases[[paste0("random_", i)]] <- list(
      sample(c(NA_integer_, 1:5), n, replace = TRUE),
      sample(c(NA_integer_, 4:9), n, replace = TRUE)
    )
  }

  for (case in cases) {
    expect_identical(
      SOMevidence:::.adjusted_rand(case[[1L]], case[[2L]]),
      .v112_adjusted_rand(case[[1L]], case[[2L]])
    )
    expect_identical(
      SOMevidence:::.adjusted_mutual_info(case[[1L]], case[[2L]]),
      .v112_adjusted_mutual_info(case[[1L]], case[[2L]])
    )
    expect_identical(
      SOMevidence:::.partition_agreement(case[[1L]], case[[2L]]),
      c(
        ari = .v112_adjusted_rand(case[[1L]], case[[2L]]),
        ami = .v112_adjusted_mutual_info(case[[1L]], case[[2L]])
      )
    )
  }
})

test_that("partition pair order and agreement values preserve version 1.1.2", {
  data <- simulate_som_scenario("clusters", n = 54L, p = 4L, seed = 11341L)
  resamples <- som_resamples(
    data,
    method = "subsample",
    repeats = 3L,
    prop = 0.8,
    seed = 11342L
  )
  specification <- som_spec(
    data.frame(xdim = c(3L, 2L), ydim = c(2L, 2L)),
    seeds = c(11344L, 11343L),
    rlen = 10L,
    k = 2:3
  )
  ensemble <- fit_som_ensemble(
    data,
    specification,
    resamples,
    keep_models = FALSE
  )
  partitions <- partition_som(ensemble, k = 2:3)

  expected <- list()
  cursor <- 0L
  for (candidate_k in 2:3) {
    records <- Filter(function(record) record$k == candidate_k, partitions$records)
    pairs <- utils::combn(seq_along(records), 2L)
    for (j in seq_len(ncol(pairs))) {
      first <- records[[pairs[1L, j]]]
      second <- records[[pairs[2L, j]]]
      jointly_observed <- !is.na(first$sample_labels) &
        !is.na(second$sample_labels)
      cursor <- cursor + 1L
      expected[[cursor]] <- data.frame(
        k = candidate_k,
        fit_a = first$id,
        fit_b = second$id,
        scope = "analysis",
        n_joint = sum(jointly_observed),
        joint_coverage = mean(jointly_observed),
        ari = .v112_adjusted_rand(first$sample_labels, second$sample_labels),
        ami = .v112_adjusted_mutual_info(
          first$sample_labels,
          second$sample_labels
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  expected <- do.call(rbind, expected)
  rownames(expected) <- NULL

  expect_identical(partitions$pairwise, expected)
})

test_that("training topographic error uses only analysis rows without drift", {
  data <- simulate_som_scenario(
    "multilayer_conflict",
    n = 60L,
    p = 3L,
    seed = 11345L
  )
  data$layers[[1L]][51:60, 1L] <- NA_real_
  data$layers[[2L]][51:60, 2L] <- 1e8
  analysis <- c(50:31, 1:30)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(list(
      id = "non_sorted",
      analysis = analysis,
      assessment = 51:60
    ))
  )
  specification <- som_spec(
    c(3L, 2L),
    seeds = c(11346L, 11347L),
    rlen = 15L,
    k = 2L,
    max_na_fraction = 0.5
  )
  ensemble <- fit_som_ensemble(
    data,
    specification,
    resamples,
    keep_models = FALSE
  )

  for (fit in ensemble$fits) {
    restored <- .v113_restore_processed_layers(fit, data)
    expect_identical(
      fit$training_topographic_error,
      .v112_topographic_error(restored, fit$analysis)
    )
    expect_null(fit$processed_all)

    legacy <- restored
    legacy$training_topographic_error <- NULL
    legacy_ensemble <- ensemble
    legacy_ensemble$fits <- list(legacy)
    legacy_ensemble$expected_models <- 1L
    expect_identical(
      audit_som(legacy_ensemble)$fit_metrics$topographic_error,
      fit$training_topographic_error
    )
  }
})
