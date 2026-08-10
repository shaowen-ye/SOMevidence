test_that("warning capture is structured and does not leak to the console", {
  captured <- expect_no_warning(SOMevidence:::.capture_warnings({
    warning("diagnostic warning")
    42
  }))

  expect_equal(captured$value, 42)
  expect_equal(nrow(captured$warnings), 1L)
  expect_equal(captured$warnings$warning_class, "simpleWarning")
  expect_equal(captured$warnings$warning, "diagnostic warning")
})

test_that("repeated warnings remain separate audit events", {
  captured <- expect_no_warning(SOMevidence:::.capture_warnings({
    warning("repeated diagnostic")
    warning("repeated diagnostic")
    42
  }))

  expect_equal(nrow(captured$warnings), 2L)
  expect_equal(
    captured$warnings$warning,
    rep("repeated diagnostic", 2L)
  )
})

test_that("SOM and reference objects always expose warning audit tables", {
  data <- simulate_som_scenario("clusters", n = 80, p = 4, seed = 91)
  spec <- som_spec(grids = c(4, 3), seeds = 2L, rlen = 30L, k = 2:3)
  ensemble <- fit_som_ensemble(data, spec, keep_models = FALSE)

  expect_s3_class(ensemble, "som_ensemble")
  expect_named(
    ensemble$warnings,
    c("id", "split_id", "grid_id", "seed", "warning_class", "warning")
  )

  references <- fit_cross_models(
    ensemble,
    methods = c("kmeans", "ward"),
    k = 2:3,
    keep_models = FALSE
  )
  expect_s3_class(references, "som_cross_models")
  expect_named(
    references$warnings,
    c(
      "id", "split_id", "method", "k", "seed", "warning_class",
      "warning"
    )
  )
  expect_error(
    fit_cross_models(
      ensemble,
      methods = "kmeans",
      k = 2,
      kmeans_iter_max = 2.5
    ),
    "must be integers"
  )
})
