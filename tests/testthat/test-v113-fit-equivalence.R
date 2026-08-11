test_that("compact tasks preserve fit and diagnostic ordering", {
  data <- simulate_som_scenario("clusters", n = 40, p = 3, seed = 3101)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(
      list(id = "alpha", analysis = 1:30, assessment = 31:40),
      list(id = "beta", analysis = 11:40, assessment = 1:10)
    )
  )
  specification <- som_spec(
    list(c(2, 2), c(3, 2)), seeds = c(3102, 3103), rlen = 2, k = 2
  )
  budget <- expand_som_spec(specification)
  expected_ids <- unlist(lapply(resamples$splits, function(split) {
    paste(split$id, budget$model_id, sep = "__")
  }), use.names = FALSE)
  call_log <- new.env(parent = emptyenv())
  call_log$keys <- character()

  local_mocked_bindings(
    .fit_one_som = function(data, analysis, spec, grid_row, seed,
                            preprocess, keep_model) {
      first_row <- analysis[[1L]]
      call_log$keys <- c(
        call_log$keys,
        paste(first_row, grid_row$grid_id, seed, sep = ":")
      )
      if ((first_row == 1L && grid_row$grid_id == 1L && seed == 3103L) ||
            (first_row == 11L && grid_row$grid_id == 1L && seed == 3102L)) {
        warning("deliberate ordered warning")
      }
      if ((first_row == 1L && grid_row$grid_id == 2L && seed == 3102L) ||
            (first_row == 11L && grid_row$grid_id == 1L && seed == 3102L)) {
        stop("deliberate ordered failure")
      }
      list(success = TRUE, analysis = analysis, model = NULL)
    },
    .package = "SOMevidence"
  )

  ensemble <- fit_som_ensemble(
    data, specification, resamples, keep_models = FALSE
  )

  expect_identical(
    call_log$keys,
    c(
      "1:1:3102", "1:1:3103", "1:2:3102", "1:2:3103",
      "11:1:3102", "11:1:3103", "11:2:3102", "11:2:3103"
    )
  )
  expect_identical(
    vapply(ensemble$fits, `[[`, character(1), "id"),
    expected_ids
  )
  expect_identical(ensemble$failures$id, expected_ids[c(3L, 5L)])
  expect_identical(ensemble$warnings$id, expected_ids[c(2L, 5L)])
  expect_true(ensemble$fits[[2L]]$success)
  expect_false(ensemble$fits[[3L]]$success)
  expect_false(ensemble$fits[[5L]]$success)
  expect_true(ensemble$fits[[6L]]$success)
})

test_that("parallel dispatch uses one compact indexed future call", {
  skip_if_not_installed("future.apply")
  data <- simulate_som_scenario("clusters", n = 40, p = 3, seed = 3111)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(
      list(id = "first", analysis = 1:30, assessment = 31:40),
      list(id = "second", analysis = 11:40, assessment = 1:10)
    )
  )
  specification <- som_spec(
    list(c(2, 2), c(3, 2)), seeds = c(3112, 3113), rlen = 2, k = 2
  )
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L

  local_mocked_bindings(
    .fit_one_som = function(data, analysis, spec, grid_row, seed,
                            preprocess, keep_model) {
      list(success = TRUE, analysis = analysis, model = NULL)
    },
    .package = "SOMevidence"
  )
  local_mocked_bindings(
    future_lapply = function(x, fun, ...) {
      observed$calls <- observed$calls + 1L
      observed$tasks <- x
      observed$dots <- list(...)
      lapply(x, fun)
    },
    .package = "future.apply"
  )

  ensemble <- fit_som_ensemble(
    data, specification, resamples,
    keep_models = FALSE, parallel = TRUE
  )

  expect_identical(observed$calls, 1L)
  expect_identical(observed$tasks, seq_len(8L))
  expect_true(is.integer(observed$tasks))
  expect_identical(observed$dots$future.seed, TRUE)
  expect_identical(
    vapply(ensemble$fits, `[[`, character(1), "split_id"),
    rep(c("first", "second"), each = 4L)
  )
})

test_that("explicit seeds remain equivalent across sequential and future fits", {
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("sequential")

  data <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 3121)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(
      list(id = "early", analysis = 1:48, assessment = 49:60),
      list(id = "late", analysis = 13:60, assessment = 1:12)
    )
  )
  specification <- som_spec(
    list(c(2, 2), c(3, 2)), seeds = c(3122, 3123), rlen = 10, k = 2
  )

  sequential <- fit_som_ensemble(
    data, specification, resamples, keep_models = FALSE
  )
  distributed <- fit_som_ensemble(
    data, specification, resamples,
    keep_models = FALSE, parallel = TRUE
  )

  expect_identical(
    vapply(sequential$fits, `[[`, character(1), "id"),
    vapply(distributed$fits, `[[`, character(1), "id")
  )
  expect_identical(
    lapply(sequential$fits, `[[`, "bmu"),
    lapply(distributed$fits, `[[`, "bmu")
  )
  expect_equal(
    lapply(sequential$fits, `[[`, "distances"),
    lapply(distributed$fits, `[[`, "distances"),
    tolerance = 0
  )
  expect_identical(sequential$failures, distributed$failures)
  expect_identical(sequential$warnings, distributed$warnings)
})

test_that("discarding models omits kohonen training data without changing results", {
  data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 3131)
  specification <- som_spec(c(3, 2), seeds = 3132, rlen = 10, k = 2)
  observed <- new.env(parent = emptyenv())
  observed$keep_data <- logical()
  original_som <- kohonen::som

  local_mocked_bindings(
    som = function(...) {
      arguments <- list(...)
      observed$keep_data <- c(observed$keep_data, arguments$keep.data)
      do.call(original_som, arguments)
    },
    .package = "kohonen"
  )

  discarded <- fit_som_ensemble(
    data, specification, keep_models = FALSE
  )
  retained <- fit_som_ensemble(
    data, specification, keep_models = TRUE
  )
  discarded_fit <- discarded$fits[[1L]]
  retained_fit <- retained$fits[[1L]]

  expect_identical(observed$keep_data, c(FALSE, TRUE))
  expect_null(discarded_fit$model)
  expect_false(is.null(retained_fit$model$data))
  for (component in c(
    "success", "analysis", "bmu", "distances",
    "training_quantization_error", "training_topographic_error",
    "empty_unit_rate", "codes", "grid", "user_weights",
    "distance_weights", "requested_layer_weights",
    "layer_mean_squared_distance", "effective_layer_weights", "whatmap"
  )) {
    expect_equal(
      discarded_fit[[component]], retained_fit[[component]],
      tolerance = 0,
      info = component
    )
  }
})

test_that("compact sequential tasks preserve the caller RNG state", {
  data <- simulate_som_scenario("clusters", n = 36L, p = 3L, seed = 3133L)
  specification <- som_spec(
    c(2L, 2L),
    seeds = c(3134L, 3135L),
    rlen = 5L,
    k = 2L
  )
  set.seed(3136L)
  before <- .Random.seed
  invisible(fit_som_ensemble(
    data,
    specification,
    keep_models = FALSE,
    parallel = FALSE
  ))
  expect_identical(.Random.seed, before)
})
