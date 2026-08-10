test_that("controlled cross-model fits reuse SOM splits and preprocessing", {
  d <- simulate_som_scenario("clusters", n = 90, p = 5, seed = 501)
  r <- som_resamples(d, method = "subsample", repeats = 2, seed = 502)
  s <- som_spec(c(3, 2), seeds = c(503, 504), rlen = 20, k = 2:3)
  e <- fit_som_ensemble(d, s, r)
  p <- partition_som(e)

  references <- fit_cross_models(
    e,
    methods = c("kmeans", "ward"), kmeans_seeds = c(505, 506)
  )
  comparison <- compare_cross_models(p, references)

  expect_equal(nrow(references$failures), 0)
  expect_setequal(unique(vapply(
    references$records, `[[`, character(1), "split_id"
  )), vapply(r$splits, `[[`, character(1), "id"))
  expect_setequal(unique(comparison$comparisons$method), c("kmeans", "ward"))
  expect_true(all(comparison$comparisons$scope == "analysis"))
  expect_true(all(comparison$summary$scope == "analysis"))
  expect_true(all(comparison$comparisons$n == 72))

  transfer_comparison <- compare_cross_models(
    p, references, scope = "assessment"
  )
  expect_true(all(transfer_comparison$comparisons$scope == "assessment"))
  expect_true(all(transfer_comparison$summary$scope == "assessment"))
  expect_true(all(transfer_comparison$comparisons$n == 18))
})

test_that("assessment comparison never falls back to analysis rows", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 513)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(514, 515), rlen = 15, k = 2)
  )
  p <- partition_som(e)
  references <- fit_cross_models(e, methods = "ward", k = 2)

  expect_error(
    compare_cross_models(p, references, scope = "assessment"),
    "no analysis fallback"
  )
})

test_that("cross-model comparison rejects unrelated source ensembles", {
  first <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 518)
  second <- simulate_som_scenario("overlap", n = 60, p = 4, seed = 519)
  specification <- som_spec(c(3, 2), seeds = 520, rlen = 15, k = 2)
  first_ensemble <- fit_som_ensemble(first, specification)
  second_ensemble <- fit_som_ensemble(second, specification)
  first_partitions <- partition_som(first_ensemble)
  second_references <- fit_cross_models(
    second_ensemble, methods = "ward", k = 2
  )

  expect_error(
    compare_cross_models(first_partitions, second_references),
    "same source ensemble"
  )
})

test_that("missing assessment rows do not invalidate complete analysis fits", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 516)
  d$layers[[1L]][51:60, 1] <- NA_real_
  r <- som_resamples(
    d,
    method = "custom",
    splits = list(list(
      id = "held_missing", analysis = 1:50, assessment = 51:60
    ))
  )
  e <- fit_som_ensemble(
    d,
    som_spec(c(3, 2), seeds = 517, rlen = 15, k = 2),
    r
  )
  p <- partition_som(e)
  references <- fit_cross_models(e, methods = c("kmeans", "ward"), k = 2)

  expect_equal(nrow(references$failures), 0)
  expect_true(all(vapply(
    references$records,
    function(record) all(!is.na(record$sample_labels[1:50])),
    logical(1)
  )))
  expect_true(all(vapply(
    references$records,
    function(record) all(is.na(record$sample_labels[51:60])),
    logical(1)
  )))
  analysis <- compare_cross_models(p, references, scope = "analysis")
  assessment <- compare_cross_models(p, references, scope = "assessment")
  expect_true(all(analysis$comparisons$n == 50))
  expect_true(all(assessment$comparisons$n == 0))
})

test_that("GMM covariance selection is visible or failure is recorded", {
  skip_if_not_installed("mclust")
  d <- simulate_som_scenario("clusters", n = 75, p = 4, seed = 507)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = 508, rlen = 15, k = 3)
  )
  references <- fit_cross_models(e, methods = "gmm", k = 3)

  expect_equal(length(references$records) + nrow(references$failures), 1)
  if (length(references$records)) {
    expect_true(nzchar(references$records[[1]]$selected_model))
  }
})

test_that("multi-layer reference methods share deterministic SOM layer weights", {
  d <- simulate_som_scenario("multilayer_conflict", n = 80, p = 4, seed = 509)
  r <- som_resamples(d, method = "subsample", repeats = 1, seed = 510)
  s <- som_spec(
    c(3, 2), seeds = c(511, 512), rlen = 15, k = 2,
    layer_weights = c(traits = 2, environment = 1)
  )
  e <- fit_som_ensemble(d, s, r)
  references <- fit_cross_models(e, methods = "ward", k = 2)

  expect_equal(nrow(references$failures), 0)
  expect_equal(
    e$fits[[1]]$effective_layer_weights,
    e$fits[[2]]$effective_layer_weights
  )
  expect_equal(
    e$fits[[1]]$effective_layer_weights,
    references$records[[1]]$effective_layer_weights
  )
})

test_that("AMI is symmetric and equals one for identical partitions", {
  x <- rep(1:3, each = 4)
  y <- c(1, 1, 2, 2, 1, 2, 2, 3, 1, 3, 3, 3)
  expect_equal(SOMevidence:::.adjusted_mutual_info(x, x), 1)
  expect_equal(
    SOMevidence:::.adjusted_mutual_info(x, y),
    SOMevidence:::.adjusted_mutual_info(y, x),
    tolerance = 1e-12
  )
})
