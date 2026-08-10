test_that("external labels remain a post hoc agreement assessment", {
  d <- simulate_som_scenario("clusters", n = 75, p = 4, seed = 801)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(802, 803), rlen = 15, k = 3)
  )
  consensus <- consensus_som(partition_som(e), k = 3)
  assessment <- evaluate_external_labels(consensus)

  expect_s3_class(assessment, "som_external_assessment")
  expect_equal(assessment$n_used, nrow(d$metadata))
  expect_true(assessment$ari >= -1 && assessment$ari <= 1)
  expect_true(assessment$ami >= -1 && assessment$ami <= 1)
  expect_equal(sum(assessment$contingency), nrow(d$metadata))
})

test_that("unknown and missing external labels can be excluded explicitly", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 804)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(805, 806), rlen = 15, k = 3)
  )
  consensus <- consensus_som(partition_som(e), k = 3)
  labels <- as.character(d$metadata$external_label)
  labels[1:2] <- NA_character_
  labels[3:4] <- "unknown"
  assessment <- evaluate_external_labels(
    consensus,
    labels = labels,
    exclude = "unknown"
  )

  expect_equal(assessment$n_omitted, 4)
  expect_identical(assessment$excluded_values, "unknown")
})

test_that("external assessment excludes samples without consensus support", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 807)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(808, 809), rlen = 15, k = 3)
  )
  consensus <- consensus_som(partition_som(e), k = 3)
  consensus$consensus_labels[1:3] <- NA_integer_
  consensus$assignment_count[1:3] <- 0L
  assessment <- evaluate_external_labels(consensus)

  expect_equal(assessment$n_used, nrow(d$metadata) - 3L)
  expect_equal(assessment$n_omitted, 3L)
  expect_equal(sum(assessment$contingency), nrow(d$metadata) - 3L)
})

test_that("external labels require replicated consensus assignments", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 810)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(811, 812), rlen = 15, k = 3)
  )
  consensus <- consensus_som(partition_som(e), k = 3)
  consensus$assignment_count[1:2] <- 1L
  assessment <- evaluate_external_labels(consensus)

  expect_equal(assessment$n_used, nrow(d$metadata) - 2L)
  expect_equal(sum(assessment$contingency), nrow(d$metadata) - 2L)
})
