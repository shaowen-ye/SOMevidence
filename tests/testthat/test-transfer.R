test_that("transfer audit quantifies held-out mapping shift", {
  d <- simulate_som_scenario("gradient", n = 90, p = 5, seed = 601)
  r <- som_resamples(d, method = "leave_domain_out", domain = "domain")
  s <- som_spec(c(3, 2), seeds = c(602, 603), rlen = 20, k = 2:3)
  e <- fit_som_ensemble(d, s, r)
  transfer <- audit_transfer(e)

  expect_equal(nrow(transfer$metrics), length(r$splits) * length(s$seeds))
  expect_true(all(transfer$metrics$n_assessment > 0))
  expect_true(all(transfer$metrics$unoccupied_unit_rate >= 0 &
                    transfer$metrics$unoccupied_unit_rate <= 1))
  expect_true(all(transfer$metrics$distance_ratio > 0))
  expect_true(all(transfer$metrics$n_analysis_mapped <=
                    transfer$metrics$n_analysis))
  expect_true(all(transfer$metrics$n_assessment_mapped <=
                    transfer$metrics$n_assessment))
  expect_true(all(transfer$metrics$assessment_mapping_coverage >= 0 &
                    transfer$metrics$assessment_mapping_coverage <= 1))
})

test_that("transfer audit exposes incomplete held-out mapping", {
  data <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 604)
  data$layers[[1L]][51:60, 1L] <- NA_real_
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(list(
      id = "held_missing", analysis = 1:50, assessment = 51:60
    ))
  )
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 605, rlen = 10, k = 2),
    resamples
  )
  transfer <- audit_transfer(ensemble)

  expect_equal(transfer$metrics$n_assessment_mapped, 0L)
  expect_equal(transfer$metrics$assessment_mapping_coverage, 0)
  expect_true(is.na(transfer$metrics$distance_ratio))
  expect_output(print(transfer), "held-out coverage")
})

test_that("transfer auditing requires assessment evidence", {
  data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 606)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 607, rlen = 10, k = 2)
  )
  expect_error(audit_transfer(ensemble), "assessment rows")
})

test_that("unoccupied-unit rates exclude unmapped observations", {
  expect_equal(SOMevidence:::.unoccupied_rate(c(NA, 2, 5), 1:3), 0.5)
  expect_true(is.na(SOMevidence:::.unoccupied_rate(c(NA, NA), 1:3)))
})
