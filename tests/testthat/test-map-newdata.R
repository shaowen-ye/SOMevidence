test_that("new observations map through every retained ensemble member", {
  full <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 1001)
  training <- som_data(
    x = full$layers$environment[1:45, , drop = FALSE],
    id = full$metadata$id[1:45]
  )
  new_data <- som_data(
    x = full$layers$environment[46:60, , drop = FALSE],
    id = full$metadata$id[46:60]
  )
  ensemble <- fit_som_ensemble(
    training,
    som_spec(c(3, 2), seeds = c(1002, 1003), rlen = 15, k = 2),
    keep_models = TRUE
  )
  mapped <- map_som_ensemble(ensemble, new_data)

  expect_s3_class(mapped, "som_newdata_mapping")
  expect_equal(nrow(mapped$summary), 2L)
  expect_equal(nrow(mapped$records), 30L)
  expect_equal(nrow(mapped$failures), 0L)
  expect_setequal(unique(mapped$records$sample_id), new_data$metadata$id)
  expect_true(all(is.finite(mapped$summary$distance_ratio)))
  expect_true(all(mapped$summary$unoccupied_unit_rate >= 0 &
                    mapped$summary$unoccupied_unit_rate <= 1))
})

test_that("one new observation and reordered variables are supported", {
  training <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 1004)
  ensemble <- fit_som_ensemble(
    training,
    som_spec(c(3, 2), seeds = 1005, rlen = 15, k = 2)
  )
  new_matrix <- training$layers$environment[1, 4:1, drop = FALSE]
  new_data <- som_data(
    layers = list(environment = new_matrix),
    id = "new_sample"
  )
  mapped <- map_som_ensemble(ensemble, new_data)

  expect_equal(nrow(mapped$records), 1L)
  expect_identical(mapped$records$sample_id, "new_sample")
})

test_that("mapping requires retained models and matching variables", {
  training <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 1006)
  specification <- som_spec(c(3, 2), seeds = 1007, rlen = 15, k = 2)
  discarded <- fit_som_ensemble(training, specification, keep_models = FALSE)
  new_data <- som_data(
    layers = list(
      environment = training$layers$environment[1:2, , drop = FALSE]
    ),
    id = c("new_1", "new_2")
  )

  expect_error(
    map_som_ensemble(discarded, new_data),
    "keep_models = TRUE"
  )

  retained <- fit_som_ensemble(training, specification, keep_models = TRUE)
  wrong <- new_data
  colnames(wrong$layers$environment)[[1L]] <- "wrong_name"
  expect_error(map_som_ensemble(retained, wrong), "variables in layer")
})
