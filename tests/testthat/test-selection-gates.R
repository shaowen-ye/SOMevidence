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

test_that("fit-level Pareto selection requires common analysis rows", {
  make_audit <- function(second_analysis) {
    structure(
      list(
        fit_metrics = data.frame(
          id = c("fit_a", "fit_b"),
          split_id = c("split_a", "split_b"),
          quantization_error = c(0.2, 0.3),
          topographic_error = c(0.1, 0.2),
          empty_unit_rate = c(0.05, 0.1)
        ),
        ensemble = list(
          resamples = list(
            splits = list(
              list(id = "split_a", analysis = 1:8),
              list(id = "split_b", analysis = second_analysis)
            )
          )
        )
      ),
      class = "som_audit"
    )
  }

  expect_warning(
    exploratory <- pareto_candidates(make_audit(3:10)),
    "distinct analysis sets"
  )
  expect_equal(nrow(exploratory), 1L)
  expect_identical(
    attr(exploratory, "comparison_scope"),
    "distinct_analysis_sets"
  )
  comparable <- pareto_candidates(make_audit(1:8))
  expect_equal(nrow(comparable), 1L)
  expect_identical(
    attr(comparable, "comparison_scope"),
    "common_analysis_set"
  )
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
  missing_partition <- assess_defensibility(
    a,
    gate = som_gate(max_topographic_error = 1)
  )
  expect_identical(missing_partition$status, "uncertain")
  missing_check <- missing_partition$checks[
    missing_partition$checks$requirement ==
      "candidate_partition_evidence_available",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(missing_check), 1L)
  expect_true(is.na(missing_check$passed))

  representation_only <- assess_defensibility(
    a, p, k = 2,
    gate = som_gate(max_topographic_error = 1, min_success_rate = 1)
  )
  expect_identical(representation_only$status, "uncertain")
  expect_true(is.na(representation_only$checks$passed[
    representation_only$checks$requirement ==
      "partition_quality_requirement_specified"
  ]))

  single_ensemble <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = 305, rlen = 15, k = 2)
  )
  single_audit <- audit_som(single_ensemble)
  single_partitions <- partition_som(single_ensemble)
  single_decision <- assess_defensibility(
    single_audit, single_partitions, k = 2,
    gate = som_gate(min_median_ari = 0)
  )
  expect_identical(single_decision$status, "uncertain")
  expect_true(is.na(single_decision$checks$passed[
    single_decision$checks$requirement ==
      "comparative_partition_evidence_available"
  ]))
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

  legacy_partitions <- p
  legacy_partitions$partition_method <- NULL
  expect_identical(
    assess_defensibility(
      a, legacy_partitions,
      k = 2, gate = gate,
      consensus = consensus, cross_model = comparison
    )$status,
    "supported"
  )

  alternative_partitions <- partition_som(e, method = "complete")
  alternative_consensus <- consensus_som(alternative_partitions, k = 2)
  alternative_comparison <- compare_cross_models(
    alternative_partitions, references
  )
  expect_error(
    assess_defensibility(
      a, p, k = 2, gate = gate,
      consensus = alternative_consensus, cross_model = comparison
    ),
    "same candidate partitions"
  )
  expect_error(
    assess_defensibility(
      a, p, k = 2, gate = gate,
      consensus = consensus, cross_model = alternative_comparison
    ),
    "same candidate partitions"
  )

  identical_records_other_method <- consensus
  identical_records_other_method$partition_method <- "complete"
  expect_identical(identical_records_other_method$records, consensus$records)
  expect_error(
    assess_defensibility(
      a, p, k = 2, gate = gate,
      consensus = identical_records_other_method,
      cross_model = comparison
    ),
    "same candidate partitions"
  )

  cross_other_method <- comparison
  cross_other_method$partition_method <- "complete"
  expect_identical(
    cross_other_method$partition_records,
    comparison$partition_records
  )
  expect_error(
    assess_defensibility(
      a, p, k = 2, gate = gate,
      consensus = consensus,
      cross_model = cross_other_method
    ),
    "same candidate partitions"
  )

  untraceable_comparison <- comparison
  untraceable_comparison$partition_records <- NULL
  expect_error(
    assess_defensibility(
      a, k = 2,
      gate = som_gate(min_cross_model_ari = 0),
      cross_model = untraceable_comparison
    ),
    "retain source-partition provenance"
  )

  untraceable_consensus <- consensus
  untraceable_consensus$partition_method <- NULL
  expect_error(
    assess_defensibility(
      a, p, k = 2, gate = gate,
      consensus = untraceable_consensus
    ),
    "retain source-partition provenance"
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
