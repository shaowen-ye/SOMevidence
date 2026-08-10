test_that("Pareto selection returns non-dominated candidates without ranking", {
  candidates <- data.frame(
    id = letters[1:4],
    qe = c(1, 2, 1.5, 3),
    stability = c(0.6, 0.9, 0.75, 0.4)
  )
  result <- pareto_candidates(
    candidates,
    metrics = c(qe = "min", stability = "max")
  )

  expect_setequal(result$id, c("a", "b", "c"))
  expect_false("rank" %in% names(result))
})

test_that("defensibility is not asserted without explicit gates", {
  d <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 301)
  s <- som_spec(c(3, 2), seeds = c(302, 303), rlen = 15, k = 2)
  e <- fit_som_ensemble(d, s)
  a <- audit_som(e)
  p <- partition_som(e)
  consensus <- consensus_som(p, k = 2)
  references <- fit_cross_models(e, methods = c("kmeans", "ward"), k = 2)
  comparison <- compare_cross_models(p, references)

  expect_identical(assess_defensibility(a, p, k = 2)$status, "not_assessed")
  expect_error(
    assess_defensibility(
      a, p, k = 3, gate = som_gate(min_success_rate = 0)
    ),
    "was not evaluated"
  )
  expect_error(som_gate(), "no universal gate")

  other_data <- simulate_som_scenario("overlap", n = 60, p = 4, seed = 304)
  other_ensemble <- fit_som_ensemble(other_data, s)
  expect_error(
    assess_defensibility(
      a, partition_som(other_ensemble), k = 2,
      gate = som_gate(min_success_rate = 0)
    ),
    "same source ensemble"
  )

  gate <- som_gate(
    max_topographic_error = 1,
    max_empty_unit_rate = 1,
    min_median_ari = 0,
    min_median_ami = 0,
    min_pairwise_coverage = 0,
    min_cluster_jaccard = 0,
    min_consensus_coverage = 0,
    min_replicated_coverage = 0,
    min_membership_support = 0,
    max_assignment_entropy = 1,
    min_cross_model_ari = 0,
    min_cross_model_methods = 2,
    min_success_rate = 1
  )
  expect_identical(
    assess_defensibility(
      a, p,
      k = 2, gate = gate,
      consensus = consensus, cross_model = comparison
    )$status,
    "supported"
  )

  legacy_consensus <- consensus
  legacy_consensus$ensemble <- NULL
  legacy_comparison <- comparison
  legacy_comparison$ensemble <- NULL
  expect_error(
    assess_defensibility(
      a, p,
      k = 2, gate = gate,
      consensus = legacy_consensus,
      cross_model = legacy_comparison
    ),
    "same source ensemble"
  )

  all_rows <- partition_som(e, scope = "all")
  all_consensus <- consensus_som(all_rows, k = 2)
  all_comparison <- compare_cross_models(all_rows, references, scope = "all")
  expect_error(
    assess_defensibility(a, all_rows, k = 2, gate = gate),
    "analysis-scoped"
  )
  expect_error(
    assess_defensibility(
      a, p, k = 2, gate = gate, consensus = all_consensus
    ),
    "analysis-scoped"
  )
  expect_error(
    assess_defensibility(
      a, p, k = 2, gate = gate, cross_model = all_comparison
    ),
    "analysis-scoped"
  )
})
