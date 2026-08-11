test_that("Ward trees are reused without changing multi-k partitions", {
  data <- simulate_som_scenario("clusters", n = 48L, p = 4L, seed = 11350L)
  data$layers[[1L]][41:48, 1L] <- NA_real_
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(
      list(id = "zeta", analysis = c(24:13, 1:12), assessment = 25:48),
      list(id = "alpha", analysis = c(36:25, 13:24), assessment = c(1:12, 37:48))
    )
  )
  specification <- som_spec(
    c(3L, 2L),
    seeds = 11351L,
    rlen = 10L,
    k = 2:4
  )
  ensemble <- fit_som_ensemble(
    data,
    specification,
    resamples,
    keep_models = FALSE
  )
  references <- fit_cross_models(
    ensemble,
    methods = "ward",
    k = c(4L, 2L, 3L, 2L),
    keep_models = TRUE
  )

  expect_identical(
    vapply(references$records, `[[`, character(1), "split_id"),
    rep(c("zeta", "alpha"), each = 3L)
  )
  expect_identical(
    vapply(references$records, `[[`, integer(1), "k"),
    rep(2:4, times = 2L)
  )

  expected <- list()
  cursor <- 0L
  for (split in resamples$splits) {
    prepared <- SOMevidence:::.reference_matrix(
      ensemble$data,
      split$analysis,
      ensemble$preprocess,
      ensemble$spec$layer_weights,
      ensemble$spec$normalize_layers
    )
    for (candidate_k in 2:4) {
      cursor <- cursor + 1L
      expected[[cursor]] <- SOMevidence:::.fit_cross_partition(
        prepared$matrix,
        split$analysis,
        "ward",
        candidate_k,
        NA_integer_,
        50L,
        100L,
        NULL
      )
    }
  }

  for (i in seq_along(expected)) {
    observed <- references$records[[i]]
    expect_identical(observed$sample_labels, expected[[i]]$sample_labels)
    expect_identical(observed$selected_model, expected[[i]]$selected_model)
    expect_identical(observed$prediction_rule, expected[[i]]$prediction_rule)
    expect_identical(observed$model$centres, expected[[i]]$model$centres)
    expect_identical(observed$model$tree$merge, expected[[i]]$model$tree$merge)
    expect_identical(observed$model$tree$height, expected[[i]]$model$tree$height)
    expect_identical(observed$model$tree$order, expected[[i]]$model$tree$order)
    expect_identical(observed$model$tree$labels, expected[[i]]$model$tree$labels)
    expect_identical(observed$model$tree$method, expected[[i]]$model$tree$method)
    expect_identical(
      observed$model$tree$dist.method,
      expected[[i]]$model$tree$dist.method
    )
  }
})

test_that("Ward tree diagnostics remain one event per requested valid k", {
  data <- simulate_som_scenario("clusters", n = 30L, p = 3L, seed = 11352L)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(list(
      id = "short",
      analysis = 1:6,
      assessment = 7:30
    ))
  )
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(2L, 2L), seeds = 11353L, rlen = 5L, k = 2L),
    resamples,
    keep_models = FALSE
  )
  original <- SOMevidence:::.fit_ward_tree
  calls <- 0L
  testthat::local_mocked_bindings(
    .fit_ward_tree = function(training) {
      calls <<- calls + 1L
      warning("fixed Ward-tree diagnostic", call. = FALSE)
      original(training)
    },
    .package = "SOMevidence"
  )

  references <- fit_cross_models(
    ensemble,
    methods = "ward",
    k = c(2L, 3L, 6L),
    keep_models = FALSE
  )

  expect_identical(calls, 1L)
  expect_identical(references$warnings$k, c(2L, 3L))
  expect_identical(
    references$warnings$warning,
    rep("fixed Ward-tree diagnostic", 2L)
  )
  expect_identical(references$failures$k, 6L)
  expect_match(
    references$failures$error,
    "more analysis rows than clusters",
    fixed = TRUE
  )
})

test_that("reference preprocessing failures still expand over all Ward k", {
  data <- som_data(matrix(c(-1, seq_len(23L)), nrow = 8L))
  specification <- som_spec(c(2L, 2L), seeds = 11354L, rlen = 5L, k = 2:3)
  ensemble <- fit_som_ensemble(
    data,
    specification,
    preprocess = som_preprocess("log1p"),
    keep_models = FALSE
  )
  references <- fit_cross_models(ensemble, methods = "ward", k = c(3L, 2L))

  expect_length(references$records, 0L)
  expect_identical(references$failures$k, 2:3)
  expect_true(all(grepl("non-negative", references$failures$error)))
})

test_that("a cached Ward-tree failure remains one failure per valid k", {
  data <- simulate_som_scenario("clusters", n = 30L, p = 3L, seed = 11355L)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(2L, 2L), seeds = 11356L, rlen = 5L, k = 2:3),
    keep_models = FALSE
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    .fit_ward_tree = function(training) {
      calls <<- calls + 1L
      stop("fixed Ward-tree failure", call. = FALSE)
    },
    .package = "SOMevidence"
  )

  references <- fit_cross_models(ensemble, methods = "ward", k = 2:3)

  expect_identical(calls, 1L)
  expect_identical(references$failures$k, 2:3)
  expect_identical(
    references$failures$error,
    rep("fixed Ward-tree failure", 2L)
  )
})

test_that("Ward caching remains lazy when an earlier method fails fast", {
  data <- simulate_som_scenario("clusters", n = 30L, p = 3L, seed = 11357L)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(2L, 2L), seeds = 11358L, rlen = 5L, k = 2L),
    keep_models = FALSE
  )
  ward_calls <- 0L
  original_ward <- SOMevidence:::.fit_ward_tree
  testthat::local_mocked_bindings(
    .fit_ward_tree = function(training) {
      ward_calls <<- ward_calls + 1L
      original_ward(training)
    },
    .fit_cross_partition = function(...) {
      stop("fixed first-method failure", call. = FALSE)
    },
    .package = "SOMevidence"
  )

  expect_error(
    fit_cross_models(
      ensemble,
      methods = c("kmeans", "ward"),
      k = 2L,
      fail_fast = TRUE
    ),
    "fixed first-method failure",
    fixed = TRUE
  )
  expect_identical(ward_calls, 0L)
})
