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

test_that("repeated analysis sets are visible in resampling diagnostics", {
  d <- som_data(
    matrix(seq_len(50), nrow = 10),
    group = rep(letters[1:5], each = 2)
  )
  expect_warning(
    r <- som_resamples(
      d,
      method = "group_subsample",
      unit = "group",
      repeats = 10,
      prop = 0.8,
      seed = 1
    ),
    "repeat an earlier analysis set"
  )

  expect_lte(r$n_unique_analysis_splits, 5L)
  expect_equal(
    nrow(r$duplicate_analysis_splits),
    length(r$splits) - r$n_unique_analysis_splits
  )
  expect_named(
    r$duplicate_analysis_splits,
    c("split_id", "duplicates_split_id")
  )
  expect_output(print(r), "unique analysis sets")
})
