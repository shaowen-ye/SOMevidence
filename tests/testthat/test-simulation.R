test_that("simulation scenarios expose known structure only as metadata", {
  clustered <- simulate_som_scenario("clusters", n = 60, p = 3, seed = 401)
  gradient <- simulate_som_scenario("gradient", n = 60, p = 3, seed = 402)

  expect_identical(clustered$id_source, "generated")
  expect_true("external_label" %in% names(clustered$metadata))
  expect_false(any(grepl("label", colnames(clustered$layers[[1]]))))
  expect_true("domain" %in% names(gradient$metadata))
  expect_true(all(is.finite(gradient$truth$latent_gradient)))
  expect_true(all(is.na(gradient$truth$class_label)))
  expect_equal(clustered$truth$class_label, clustered$metadata$external_label)
})

test_that("simulation identifiers are explicit only when supplied", {
  ids <- paste0("paired_", seq_len(60))
  paired <- simulate_som_scenario(
    "clusters", n = 60, p = 3, seed = 405, id = ids
  )

  expect_identical(paired$id_source, "provided")
  expect_identical(paired$metadata$id, ids)
})

test_that("simulation factors are explicit and reproducible", {
  first <- simulate_som_scenario(
    "clusters", n = 90, p = 4, seed = 403,
    class_probs = c(0.7, 0.2, 0.1), domain_shift = 1.5,
    missing_rate = 0.1, missing_mechanism = "domain"
  )
  second <- simulate_som_scenario(
    "clusters", n = 90, p = 4, seed = 403,
    class_probs = c(0.7, 0.2, 0.1), domain_shift = 1.5,
    missing_rate = 0.1, missing_mechanism = "domain"
  )

  expect_identical(first$layers, second$layers)
  expect_equal(
    as.numeric(prop.table(table(first$truth$class_label))),
    c(0.7, 0.2, 0.1)
  )
  expect_equal(length(unique(first$metadata$domain)), 3L)
  expect_gt(first$simulation_spec$missing_rate_realized, 0)
  expect_true(all(rowSums(!is.na(first$layers[[1]])) >= 2L))
})

test_that("grouped simulation assigns one latent class to each group", {
  grouped <- simulate_som_scenario(
    "grouped_pseudoreplication", n = 120, p = 4, seed = 404,
    n_groups = 20, group_icc = 0.5
  )
  labels_per_group <- tapply(
    grouped$truth$class_label,
    grouped$metadata$group,
    function(x) length(unique(x))
  )

  expect_true(all(labels_per_group == 1L))
  expect_equal(length(unique(grouped$metadata$group)), 20L)
  expect_equal(grouped$simulation_spec$group_icc, 0.5)
})

test_that("invalid simulation controls fail clearly", {
  expect_error(
    simulate_som_scenario("clusters", missing_rate = 0.1),
    "missingness mechanism"
  )
  expect_error(
    simulate_som_scenario("clusters", class_probs = c(1, 0, 1)),
    "three positive"
  )
})
