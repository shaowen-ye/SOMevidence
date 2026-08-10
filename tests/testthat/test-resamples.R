test_that("group resampling preserves sampling units", {
  d <- simulate_som_scenario(
    "grouped_pseudoreplication",
    n = 96, p = 4, seed = 101
  )
  r <- som_resamples(
    d,
    method = "group_subsample", unit = "group",
    repeats = 8, prop = 0.7, seed = 102
  )

  intact <- vapply(r$splits, function(split) {
    analysis_group <- unique(d$metadata$group[split$analysis])
    assessment_group <- unique(d$metadata$group[split$assessment])
    length(intersect(analysis_group, assessment_group)) == 0L
  }, logical(1))
  expect_true(all(intact))
})

test_that("leave-domain-out produces one assessment domain per split", {
  d <- simulate_som_scenario("gradient", n = 90, p = 4, seed = 103)
  r <- som_resamples(d, method = "leave_domain_out", domain = "domain")

  expect_equal(length(r$splits), length(unique(d$metadata$domain)))
  expect_true(all(vapply(r$splits, function(split) {
    length(unique(d$metadata$domain[split$assessment])) == 1L
  }, logical(1))))
})
