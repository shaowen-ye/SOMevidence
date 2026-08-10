test_that("workflow keeps evidence streams separate", {
  d <- simulate_som_scenario("clusters", n = 75, p = 4, seed = 901)
  workflow <- run_som_workflow(
    d,
    som_spec(c(3, 2), seeds = c(902, 903), rlen = 15, k = 2:3),
    cross_models = c("kmeans", "ward"),
    cross_model_control = list(kmeans_iter_max = 75L)
  )

  expect_s3_class(workflow, "som_workflow")
  expect_named(workflow$consensus, c("k2", "k3"))
  expect_equal(nrow(workflow$consensus_failures), 0)
  expect_setequal(
    unique(workflow$cross_comparison$comparisons$method),
    c("kmeans", "ward")
  )
  expect_error(
    run_som_workflow(
      d,
      som_spec(c(3, 2), seeds = 902, rlen = 10, k = 2),
      cross_model_control = list(methods = "ward")
    ),
    "containing only"
  )
})

test_that("sensitivity scenarios are summarized without a ranking", {
  d <- simulate_som_scenario(
    "overlap", n = 75, p = 4, seed = 904,
    id = paste0("sensitivity_", seq_len(75))
  )
  common_spec <- som_spec(c(3, 2), seeds = c(905, 906), rlen = 15, k = 2)
  sensitivity <- run_som_sensitivity(
    list(
      standardized = list(
        data = d,
        spec = common_spec,
        preprocess = som_preprocess(),
        keep_models = FALSE
      ),
      unscaled = list(
        data = d,
        spec = common_spec,
        preprocess = som_preprocess(center = FALSE, scale = FALSE)
      )
    ),
    cross_models = "ward"
  )

  expect_s3_class(sensitivity, "som_sensitivity")
  expect_setequal(
    unique(sensitivity$partition$scenario),
    c("standardized", "unscaled")
  )
  expect_false("score" %in% names(sensitivity$partition))
  expect_equal(nrow(sensitivity$scenario_comparison), 1)
  expect_equal(sensitivity$scenario_comparison$k, 2)
  expect_equal(sensitivity$scenario_comparison$n_shared, 75)
  expect_equal(sensitivity$scenario_comparison$n_joint, 75)
  expect_equal(sensitivity$scenario_comparison$joint_coverage, 1)
  expect_equal(sensitivity$scenario_comparison$comparison_status, "evaluated")
  expect_equal(nrow(sensitivity$sample_comparison), 75)
  expect_equal(nrow(sensitivity$sample_summary), 75)
  expect_true(all(sensitivity$sample_summary$n_contrasts == 1L))
  expect_true(all(sensitivity$sample_summary$n_comparable_contrasts == 1L))
  expect_true(all(sensitivity$sample_summary$n_possible_contrasts == 1L))
  expect_true(all(sensitivity$sample_summary$contrast_coverage == 1))
  expect_true(all(
    sensitivity$sample_summary$conditional_contrast_coverage == 1
  ))
  expect_true(all(
    sensitivity$sample_comparison$membership_jaccard_shared >= 0 &
      sensitivity$sample_comparison$membership_jaccard_shared <= 1
  ))
  expect_true(all(
    sensitivity$sample_comparison$membership_jaccard_all >= 0 &
      sensitivity$sample_comparison$membership_jaccard_all <= 1
  ))
  expect_true(all(
    sensitivity$sample_comparison$membership_jaccard_all <=
      sensitivity$sample_comparison$membership_jaccard_shared
  ))
  modified <- sensitivity$workflows
  modified$standardized$consensus$k2$assignment_count[[1L]] <- 1L
  restricted <- .compare_sensitivity_consensus(modified)
  expect_equal(restricted$n_joint, 74)
  expect_equal(restricted$joint_coverage, 74 / 75)

  singleton <- sensitivity$workflows
  singleton$standardized$consensus$k2$assignment_count[] <- 1L
  singleton$unscaled$consensus$k2$assignment_count[] <- 1L
  singleton$standardized$consensus$k2$assignment_count[[1L]] <- 2L
  singleton$unscaled$consensus$k2$assignment_count[[1L]] <- 2L
  singleton_comparison <- .compare_sensitivity_consensus(singleton)
  expect_equal(singleton_comparison$n_joint, 1L)
  expect_equal(
    singleton_comparison$comparison_status,
    "insufficient_joint_assignments"
  )
  expect_true(is.na(singleton_comparison$ari))
  expect_true(is.na(singleton_comparison$ami))
  singleton_samples <- .compare_sensitivity_samples(
    singleton, singleton_comparison
  )
  expect_equal(nrow(singleton_samples$comparison), 0L)
  expect_equal(nrow(singleton_samples$summary), 0L)

  unavailable <- modified
  unavailable$unscaled$consensus$k2 <- NULL
  unavailable$unscaled$consensus_failures <- data.frame(
    k = 2L, error = "deliberate test failure"
  )
  unavailable_comparison <- .compare_sensitivity_consensus(unavailable)
  expect_equal(nrow(unavailable_comparison), 1)
  expect_equal(unavailable_comparison$k, 2)
  expect_equal(
    unavailable_comparison$comparison_status,
    "consensus_unavailable_b"
  )
  expect_true(all(is.na(unavailable_comparison[, c("n_joint", "ari", "ami")])))

  unstable_ids <- modified
  unstable_ids$standardized$ensemble$data$id_source <- "generated"
  unstable_ids$unscaled$ensemble$data$id_source <- "generated"
  unstable_ids$unscaled$ensemble$data$layers[[1L]] <-
    unstable_ids$unscaled$ensemble$data$layers[[1L]][75:1, , drop = FALSE]
  unstable_comparison <- .compare_sensitivity_consensus(unstable_ids)
  expect_equal(
    unstable_comparison$comparison_status,
    "unstable_generated_ids"
  )
  expect_true(all(is.na(unstable_comparison[, c("n_joint", "ari", "ami")])))

  disjoint_k <- modified
  renamed <- disjoint_k$unscaled$consensus$k2
  renamed$k <- 3L
  disjoint_k$unscaled$consensus <- list(k3 = renamed)
  disjoint_k$unscaled$partitions$stability$k <- 3L
  no_common_k <- .compare_sensitivity_consensus(disjoint_k)
  expect_equal(nrow(no_common_k), 1)
  expect_true(is.na(no_common_k$k))
  expect_equal(no_common_k$comparison_status, "no_common_k")
  expect_equal(nrow(sensitivity$failures), 0)
})

test_that("sensitivity print counts scenario pairs rather than k rows", {
  example <- structure(
    list(
      partition = data.frame(scenario = c("a", "b")),
      scenario_comparison = data.frame(
        scenario_a = rep("a", 3),
        scenario_b = rep("b", 3),
        k = 2:4,
        comparison_status = rep("evaluated", 3)
      ),
      failures = data.frame()
    ),
    class = "som_sensitivity"
  )

  printed <- capture.output(print(example))
  expect_true(any(grepl("evaluable contrasts: 1", printed, fixed = TRUE)))
  expect_true(any(grepl("unevaluable pairs   : 0", printed, fixed = TRUE)))

  example$scenario_comparison$comparison_status <- "no_shared_samples"
  unavailable <- capture.output(print(example))
  expect_true(any(grepl("evaluable contrasts: 0", unavailable, fixed = TRUE)))
  expect_true(any(grepl("unevaluable pairs   : 1", unavailable, fixed = TRUE)))
})

test_that("workflow preserves structured evidence when every SOM fit fails", {
  d <- simulate_som_scenario("clusters", n = 30, p = 4, seed = 907)
  workflow <- run_som_workflow(
    d,
    som_spec(c(6, 6), seeds = 908, rlen = 10, k = 2),
    cross_models = character(),
    fail_fast = FALSE
  )

  expect_s3_class(workflow, "som_workflow")
  expect_equal(nrow(workflow$ensemble$failures), 1)
  expect_equal(workflow$audit$success_rate, 0)
  expect_equal(nrow(workflow$audit$fit_metrics), 0)
  expect_length(workflow$partitions$records, 0)
  expect_equal(nrow(workflow$consensus_failures), 1)
  expect_match(workflow$consensus_failures$error, "at least two partitions")

  sensitivity <- run_som_sensitivity(
    list(all_failed = list(
      data = d,
      spec = som_spec(c(6, 6), seeds = 909, rlen = 10, k = 2)
    )),
    cross_models = character(),
    keep_workflows = TRUE
  )
  expect_equal(nrow(sensitivity$failures), 1)
  expect_match(sensitivity$failures$error, "All SOM fits failed")
  expect_equal(
    sensitivity$workflows$all_failed$audit$success_rate,
    0
  )

  compact <- run_som_sensitivity(
    list(all_failed = list(
      data = d,
      spec = som_spec(c(6, 6), seeds = 910, rlen = 10, k = 2)
    )),
    cross_models = character(),
    keep_workflows = FALSE
  )
  expect_equal(compact$failures$scenario, "all_failed")
  expect_equal(nrow(compact$scenario_comparison), 0)
  expect_null(compact$workflows)
})

test_that("sensitivity agreement uses identifiers and reports absent overlap", {
  simulated <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 911)
  full <- som_data(
    layers = simulated$layers,
    id = paste0("observation_", seq_len(60))
  )
  subset_rows <- 11:60
  subset <- som_data(
    full$layers[[1L]][subset_rows, , drop = FALSE],
    id = full$metadata$id[subset_rows]
  )
  disjoint <- som_data(
    full$layers[[1L]],
    id = paste0("other_", seq_len(60))
  )
  specification <- som_spec(
    c(3, 2), seeds = c(912, 913), rlen = 10, k = 2
  )
  sensitivity <- run_som_sensitivity(
    list(
      full = list(data = full, spec = specification),
      subset = list(data = subset, spec = specification),
      disjoint = list(data = disjoint, spec = specification)
    ),
    cross_models = character()
  )

  comparison <- sensitivity$scenario_comparison
  overlap <- comparison[
    comparison$scenario_a == "full" & comparison$scenario_b == "subset", ,
    drop = FALSE
  ]
  expect_equal(overlap$n_shared, 50)
  expect_equal(overlap$n_joint, 50)
  expect_equal(overlap$comparison_status, "evaluated")
  no_overlap <- comparison[comparison$scenario_b == "disjoint", , drop = FALSE]
  expect_true(all(no_overlap$n_shared == 0))
  expect_true(all(no_overlap$comparison_status == "no_shared_samples"))
  expect_true(all(is.na(no_overlap$ari)))
  expect_true(all(is.na(no_overlap$ami)))
  sample_profile <- sensitivity$sample_summary
  expect_equal(nrow(sample_profile), 50)
  expect_true(all(sample_profile$n_contrasts == 1L))
  expect_true(all(sample_profile$n_comparable_contrasts == 1L))
  expect_true(all(sample_profile$n_possible_contrasts == 3L))
  expect_true(all(sample_profile$contrast_coverage == 1 / 3))
  expect_true(all(sample_profile$conditional_contrast_coverage == 1))

  legacy <- sensitivity$workflows
  legacy$full$ensemble$data$id_source <- NULL
  legacy$subset$ensemble$data$id_source <- NULL
  legacy_comparison <- .compare_sensitivity_consensus(legacy[c("full", "subset")])
  expect_equal(
    legacy_comparison$comparison_status,
    "id_provenance_unavailable"
  )
  expect_true(is.na(legacy_comparison$n_shared))
})

test_that("sample-level sensitivity is invariant to cluster label permutation", {
  ids <- paste0("sample_", seq_len(4))
  first <- list(
    metadata = data.frame(id = ids),
    consensus_labels = c(1L, 1L, 2L, 2L),
    assignment_count = rep(2L, 4)
  )
  permuted <- list(
    metadata = data.frame(id = ids),
    consensus_labels = c(2L, 2L, 1L, 1L),
    assignment_count = rep(2L, 4)
  )
  changed <- list(
    metadata = data.frame(id = ids),
    consensus_labels = c(2L, 1L, 1L, 1L),
    assignment_count = rep(2L, 4)
  )

  unchanged <- .sample_membership_jaccard(
    c("a", "b"), 2L, first, permuted
  )
  expect_equal(unchanged$membership_jaccard_shared, rep(1, 4))
  expect_equal(unchanged$membership_jaccard_all, rep(1, 4))

  contrasted <- .sample_membership_jaccard(
    c("a", "c"), 2L, first, changed
  )
  expected <- c(0.5, 0.25, 2 / 3, 2 / 3)
  expect_equal(contrasted$membership_jaccard_shared, expected)
  expect_equal(contrasted$membership_jaccard_all, expected)
  expect_equal(contrasted$cluster_intersection, c(1L, 1L, 2L, 2L))
  expect_equal(contrasted$shared_cluster_union, c(2L, 4L, 3L, 3L))
  expect_equal(contrasted$all_cluster_union, c(2L, 4L, 3L, 3L))
})

test_that("sample profiles separate repartitioning from coverage changes", {
  first <- list(
    metadata = data.frame(id = paste0("id_", 1:4)),
    consensus_labels = c(1L, 1L, 2L, 2L),
    assignment_count = rep(2L, 4)
  )
  subset <- list(
    metadata = data.frame(id = paste0("id_", 1:3)),
    consensus_labels = c(2L, 2L, 1L),
    assignment_count = rep(2L, 3)
  )

  comparison <- .sample_membership_jaccard(
    c("full", "subset"), 2L, first, subset
  )
  expect_equal(comparison$membership_jaccard_shared, rep(1, 3))
  expect_equal(comparison$membership_jaccard_all, c(1, 1, 0.5))
  expect_equal(comparison$shared_cluster_size_a, c(2L, 2L, 1L))
  expect_equal(comparison$cluster_size_a, c(2L, 2L, 2L))

  singleton <- subset
  singleton$metadata <- singleton$metadata[1, , drop = FALSE]
  singleton$consensus_labels <- singleton$consensus_labels[[1L]]
  singleton$assignment_count <- 2L
  expect_equal(
    nrow(.sample_membership_jaccard(
      c("full", "singleton"), 2L, first, singleton
    )),
    0L
  )
})

test_that("sample profiles can be disabled for large sensitivity designs", {
  d <- simulate_som_scenario(
    "clusters", n = 40, p = 3, seed = 914,
    id = paste0("profile_control_", seq_len(40))
  )
  specification <- som_spec(
    c(3, 2), seeds = c(915, 916), rlen = 10, k = 2
  )
  result <- run_som_sensitivity(
    list(
      first = list(data = d, spec = specification),
      second = list(data = d, spec = specification)
    ),
    cross_models = character(),
    sample_profiles = FALSE,
    keep_workflows = FALSE
  )

  expect_equal(nrow(result$scenario_comparison), 1L)
  expect_equal(nrow(result$sample_comparison), 0L)
  expect_equal(nrow(result$sample_summary), 0L)
})

test_that("failed scenarios remain in planned contrast denominators", {
  d <- simulate_som_scenario(
    "clusters", n = 40, p = 3, seed = 917,
    id = paste0("failed_scenario_", seq_len(40))
  )
  specification <- som_spec(
    c(3, 2), seeds = c(918, 919), rlen = 10, k = 2
  )
  result <- run_som_sensitivity(
    list(
      first = list(data = d, spec = specification),
      second = list(data = d, spec = specification),
      failed = list(data = "not_som_data", spec = specification)
    ),
    cross_models = character(),
    keep_workflows = FALSE,
    fail_fast = FALSE
  )

  expect_equal(nrow(result$failures), 1L)
  expect_equal(result$failures$scenario, "failed")
  expect_equal(nrow(result$scenario_comparison), 3L)
  expect_equal(
    sum(result$scenario_comparison$comparison_status == "evaluated"),
    1L
  )
  expect_equal(
    sum(grepl(
      "^scenario_failed_",
      result$scenario_comparison$comparison_status
    )),
    2L
  )
  expect_true(all(result$sample_summary$n_comparable_contrasts == 1L))
  expect_true(all(result$sample_summary$n_possible_contrasts == 3L))
  expect_true(all(result$sample_summary$contrast_coverage == 1 / 3))
  expect_true(all(
    result$sample_summary$conditional_contrast_coverage == 1
  ))
})

test_that("sensitivity k extraction is exact and malformed k is not coerced", {
  d <- simulate_som_scenario(
    "clusters", n = 40, p = 3, seed = 920,
    id = paste0("exact_k_", seq_len(40))
  )
  specification <- som_spec(
    c(3, 2), seeds = c(921, 922), rlen = 10, k = 2:3
  )

  expect_equal(
    .sensitivity_requested_k(list(
      data = d, spec = specification, keep_models = FALSE
    )),
    2:3
  )
  expect_equal(
    .sensitivity_requested_k(list(data = d, spec = specification, k = 3L)),
    3L
  )
  expect_length(
    .sensitivity_requested_k(list(data = d, spec = specification, k = 2.9)),
    0L
  )
  expect_length(
    .sensitivity_requested_k(list(
      data = d, spec = specification, k = c("2", "bad")
    )),
    0L
  )
  expect_length(
    .sensitivity_requested_k(list(
      data = d, spec = specification, k = integer()
    )),
    0L
  )

  malformed <- run_som_sensitivity(
    list(
      valid = list(data = d, spec = specification, k = 2L),
      malformed = list(data = d, spec = specification, k = 2.9)
    ),
    cross_models = character(),
    keep_workflows = FALSE,
    fail_fast = FALSE
  )
  expect_equal(malformed$failures$scenario, "malformed")
  expect_equal(nrow(malformed$scenario_comparison), 1L)
  expect_true(is.na(malformed$scenario_comparison$k))
  expect_equal(
    malformed$scenario_comparison$comparison_status,
    "scenario_failed_b"
  )
})
