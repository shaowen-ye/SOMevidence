test_that("single-layer ensembles fit, audit and partition reproducibly", {
  d <- simulate_som_scenario("clusters", n = 75, p = 5, seed = 201)
  r <- som_resamples(d, method = "subsample", repeats = 2, seed = 202)
  s <- som_spec(
    grids = list(c(3, 2), c(4, 3)), seeds = c(203, 204),
    rlen = 20, k = 2:3
  )

  e1 <- fit_som_ensemble(d, s, r)
  e2 <- fit_som_ensemble(d, s, r)
  expect_equal(nrow(e1$failures), 0)
  expect_equal(
    lapply(e1$fits, `[[`, "bmu"),
    lapply(e2$fits, `[[`, "bmu")
  )

  a <- audit_som(e1)
  p <- partition_som(e1)
  expect_equal(a$success_rate, 1)
  expect_true(all(c(
    "quantization_error", "topographic_error", "empty_unit_rate"
  ) %in% names(a$fit_metrics)))
  expect_true(all(p$pairwise$ari >= -1 & p$pairwise$ari <= 1))
  expect_true(all(p$pairwise$ami >= -1 & p$pairwise$ami <= 1))
  expect_identical(p$scope, "analysis")
  for (record in p$records) {
    expect_true(all(is.na(record$sample_labels[record$assessment])))
    expect_true(all(!is.na(record$mapped_labels[record$assessment])))
  }
  expect_true(all(p$pairwise$n_joint <= nrow(d$metadata)))

  consensus <- consensus_som(p, k = 3)
  expect_null(consensus$coassignment)
  expect_true(all(consensus$membership_support >= 0 &
                    consensus$membership_support <= 1, na.rm = TRUE))
  expect_true(all(consensus$assignment_entropy >= 0 &
                    consensus$assignment_entropy <= 1 + 1e-12, na.rm = TRUE))
  expect_identical(consensus$method, "aligned_vote")
  expect_identical(consensus$scope, "analysis")
  expect_true(all(consensus$assignment_count >= 0L))
  expect_equal(
    consensus$assignment_coverage,
    mean(consensus$assignment_count > 0L)
  )
  expect_equal(
    consensus$replicated_assignment_coverage,
    mean(consensus$assignment_count >= 2L)
  )
  expect_equal(nrow(consensus$metadata), nrow(d$metadata))
})

test_that("complete-data partitions permit co-assignment consensus", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 214)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(215, 216), rlen = 15, k = 3)
  )
  consensus <- consensus_som(partition_som(e), k = 3)

  expect_identical(consensus$method, "coassignment")
  expect_equal(dim(consensus$coassignment), rep(nrow(d$metadata), 2))
  expect_true(all(consensus$assignment_count == 2L))
})

test_that("single assignments do not masquerade as stable consensus", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 217)
  r <- som_resamples(
    d,
    method = "custom",
    splits = list(
      list(id = "a", analysis = 1:40),
      list(id = "b", analysis = 21:60)
    )
  )
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = 218, rlen = 15, k = 3), r
  )
  p <- partition_som(e)
  consensus <- consensus_som(p, k = 3)

  expect_equal(consensus$assignment_coverage, 1)
  expect_equal(consensus$replicated_assignment_coverage, 1 / 3)
  expect_true(all(is.na(consensus$membership_support[c(1:20, 41:60)])))
  expect_true(all(is.na(consensus$assignment_entropy[c(1:20, 41:60)])))
  expect_true(all(consensus$clusterwise_jaccard$n_joint == 20L))
  expect_equal(p$stability$median_joint_coverage, 1 / 3)
})

test_that("disconnected partition overlap cannot form pseudo-consensus", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 219)
  r <- som_resamples(
    d,
    method = "custom",
    splits = list(
      list(id = "a", analysis = 1:30),
      list(id = "b", analysis = 31:60)
    )
  )
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = 220, rlen = 15, k = 3), r
  )

  expect_error(
    consensus_som(partition_som(e), k = 3),
    "overlap graph is disconnected"
  )
})

test_that("large-sample consensus can avoid a dense co-assignment matrix", {
  d <- simulate_som_scenario("clusters", n = 75, p = 4, seed = 207)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(208, 209), rlen = 15, k = 3)
  )
  consensus <- consensus_som(
    partition_som(e),
    k = 3,
    method = "auto",
    max_coassignment_n = 50
  )

  expect_identical(consensus$method, "aligned_vote")
  expect_null(consensus$coassignment)
  expect_length(consensus$consensus_labels, nrow(d$metadata))
})

test_that("multi-layer supersom ensembles are supported", {
  d <- simulate_som_scenario("multilayer_conflict", n = 60, p = 4, seed = 205)
  s <- som_spec(
    c(3, 2),
    seeds = 206, rlen = 20,
    layer_weights = c(traits = 1, environment = 1), k = 2
  )
  e <- fit_som_ensemble(d, s)

  expect_equal(nrow(e$failures), 0)
  expect_length(e$fits[[1]]$codes, 2)
  expect_equal(length(e$fits[[1]]$bmu), nrow(d$metadata))
  expect_equal(sum(e$fits[[1]]$effective_layer_weights), 1)
  expect_equal(
    unname(e$fits[[1]]$effective_layer_weights),
    e$fits[[1]]$user_weights
  )
})

test_that("model failures are visible", {
  d <- som_data(matrix(c(-1, 1:11), nrow = 6))
  s <- som_spec(c(2, 2), seeds = c(1, 2), rlen = 10, k = 2)
  e <- fit_som_ensemble(d, s, preprocess = som_preprocess("log1p"))

  expect_equal(nrow(e$failures), 2)
  expect_true(all(grepl("non-negative", e$failures$error)))
  partitions <- partition_som(e, max_pairwise_comparisons = 17L)
  expect_identical(partitions$max_pairwise_comparisons, 17L)
  expect_true(all(c(
    "records", "pairwise", "stability", "method", "scope",
    "max_pairwise_comparisons", "ensemble"
  ) %in% names(partitions)))
})

test_that("future execution preserves explicitly seeded SOM results", {
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("sequential")

  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 211)
  spec <- som_spec(c(3, 2), seeds = c(212, 213), rlen = 15, k = 2)
  sequential <- fit_som_ensemble(d, spec, keep_models = FALSE)
  distributed <- fit_som_ensemble(
    d, spec, keep_models = FALSE, parallel = TRUE
  )

  expect_identical(
    lapply(sequential$fits, `[[`, "bmu"),
    lapply(distributed$fits, `[[`, "bmu")
  )
  expect_equal(
    lapply(sequential$fits, `[[`, "distances"),
    lapply(distributed$fits, `[[`, "distances")
  )
  expect_false(sequential$parallel)
  expect_true(distributed$parallel)
})
