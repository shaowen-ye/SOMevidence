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
})

test_that("unoccupied-unit rates exclude unmapped observations", {
  expect_equal(SOMevidence:::.unoccupied_rate(c(NA, 2, 5), 1:3), 0.5)
  expect_true(is.na(SOMevidence:::.unoccupied_rate(c(NA, NA), 1:3)))
})
