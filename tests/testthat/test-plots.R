test_that("audit and consensus plots return editable ggplot objects", {
  skip_if_not_installed("ggplot2")
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 701)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(702, 703), rlen = 15, k = 2)
  )
  audit <- audit_som(e)
  partitions <- partition_som(e)
  consensus <- consensus_som(partitions, k = 2)

  expect_s3_class(plot(audit), "ggplot")
  expect_s3_class(plot(partitions), "ggplot")
  expect_s3_class(plot(consensus), "ggplot")
  expect_s3_class(plot(consensus, type = "heatmap"), "ggplot")
  expect_s3_class(
    plot_som_plane(e, fit_id = e$fits[[1L]]$id, variables = c("environment_01")),
    "ggplot"
  )
  expect_s3_class(
    plot_som_plane(e, type = "occupancy", fit_id = e$fits[[1L]]$id),
    "ggplot"
  )
  expect_s3_class(
    plot_som_plane(e, type = "neighbour_distance", fit_id = e$fits[[1L]]$id),
    "ggplot"
  )

  sensitivity_data <- simulate_som_scenario(
    "clusters", n = 60, p = 4, seed = 713,
    id = paste0("plot_sensitivity_", seq_len(60))
  )
  sensitivity_spec <- som_spec(
    c(3, 2), seeds = c(714, 715), rlen = 10, k = 2
  )
  sensitivity <- run_som_sensitivity(
    list(
      scaled = list(data = sensitivity_data, spec = sensitivity_spec),
      unscaled = list(
        data = sensitivity_data,
        spec = sensitivity_spec,
        preprocess = som_preprocess(center = FALSE, scale = FALSE)
      )
    ),
    cross_models = character(),
    keep_workflows = FALSE
  )
  expect_s3_class(plot(sensitivity), "ggplot")
  expect_s3_class(plot(sensitivity, k = 2), "ggplot")
  expect_error(plot(sensitivity, k = 1), "at least 2")
})

test_that("a plane never silently selects one fit from an ensemble", {
  skip_if_not_installed("ggplot2")
  d <- simulate_som_scenario("clusters", n = 60, p = 3, seed = 707)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(708, 709), rlen = 10, k = 2)
  )

  expect_error(plot_som_plane(e), "Select `fit_id` explicitly")
})

test_that("cross-model and transfer plots return editable ggplot objects", {
  skip_if_not_installed("ggplot2")
  d <- simulate_som_scenario("gradient", n = 75, p = 4, seed = 704)
  r <- som_resamples(d, method = "leave_domain_out", domain = "domain")
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(705, 706), rlen = 15, k = 2), r
  )
  partitions <- partition_som(e)
  references <- fit_cross_models(e, methods = c("kmeans", "ward"), k = 2)

  expect_s3_class(plot(compare_cross_models(partitions, references)), "ggplot")
  expect_s3_class(plot(audit_transfer(e)), "ggplot")
})

test_that("new-data mapping plots return editable ggplot objects", {
  skip_if_not_installed("ggplot2")
  training <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 710)
  e <- fit_som_ensemble(
    training, som_spec(c(3, 2), seeds = c(711, 712), rlen = 15, k = 2)
  )
  new_data <- som_data(
    layers = list(
      environment = training$layers$environment[1:5, , drop = FALSE]
    ),
    id = paste0("new_", 1:5)
  )

  expect_s3_class(plot(map_som_ensemble(e, new_data)), "ggplot")
})
