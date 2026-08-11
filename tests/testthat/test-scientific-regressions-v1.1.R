test_that("multi-layer rows are aligned by sample identity", {
  first <- matrix(1:6, nrow = 3, dimnames = list(c("A", "B", "C"), c("x", "y")))
  second <- matrix(
    c(30, 10, 20, 300, 100, 200), nrow = 3,
    dimnames = list(c("C", "A", "B"), c("u", "v"))
  )
  data <- som_data(layers = list(first = first, second = second))

  expect_identical(data$metadata$id, c("A", "B", "C"))
  expect_identical(rownames(data$layers$second), c("A", "B", "C"))
  expect_equal(data$layers$second[, "u"], c(A = 10, B = 20, C = 30))

  mismatched <- second
  rownames(mismatched)[[1L]] <- "D"
  expect_error(
    som_data(layers = list(first = first, second = mismatched)),
    "do not match"
  )

  unnamed <- unname(second)
  expect_error(
    som_data(layers = list(first = first, second = unnamed)),
    "row identity is ambiguous"
  )

  explicit_id <- c("C", "A", "B")
  single <- som_data(x = first, id = explicit_id)
  multi <- som_data(
    layers = list(first = first, second = second),
    id = explicit_id
  )
  expect_identical(single$metadata$id, multi$metadata$id)
  expect_equal(unname(single$layers$data), unname(multi$layers$first))
  expect_equal(unname(multi$layers$first[, "x"]), c(1, 2, 3))
})

test_that("label alignment never invents unsupported correspondences", {
  aligned <- SOMevidence:::.align_labels(c(1L, 2L), c(1L, 1L), 2L)
  expect_identical(aligned, c(1L, NA_integer_))

  records <- list(
    list(
      id = "reference",
      sample_labels = c(1L, 1L, 1L, 1L, 2L, 2L, NA, NA)
    ),
    list(
      id = "invalid",
      sample_labels = c(1L, 2L, 1L, 2L, NA, NA, 1L, 2L)
    ),
    list(
      id = "valid",
      sample_labels = c(1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L)
    )
  )
  propagated <- SOMevidence:::.propagate_alignment(records, 1L, 2L)
  expect_true(all(
    propagated$alignment_diagnostics$n_resolved_clusters == 2L
  ))
})

test_that("Ward training labels remain the Ward.D2 cutree partition", {
  set.seed(1101)
  x <- matrix(stats::rnorm(80), nrow = 20)
  fitted <- SOMevidence:::.fit_cross_partition(
    x = x,
    analysis = seq_len(nrow(x)),
    method = "ward",
    k = 3L,
    seed = NA_integer_,
    kmeans_nstart = 10L,
    kmeans_iter_max = 50L,
    gmm_model_names = NULL
  )

  expect_identical(
    fitted$sample_labels,
    as.integer(stats::cutree(fitted$model$tree, k = 3L))
  )
  expect_match(fitted$prediction_rule, "Ward.D2 cutree", fixed = TRUE)
})

test_that("GMM initialization is reproducible when stochastic subsetting is used", {
  skip_if_not_installed("mclust")
  set.seed(1102)
  x <- rbind(
    matrix(stats::rnorm(3150, -2), ncol = 3),
    matrix(stats::rnorm(3150, 2), ncol = 3)
  )
  fit_once <- function(global_seed) {
    set.seed(global_seed)
    SOMevidence:::.fit_cross_partition(
      x, seq_len(nrow(x)), "gmm", 2L, 1103L,
      10L, 50L, NULL
    )
  }

  first <- fit_once(1L)
  second <- fit_once(999L)
  expect_identical(first$sample_labels, second$sample_labels)
  expect_identical(first$selected_model, second$selected_model)

  set.seed(1104)
  rng_before <- .Random.seed
  invisible(SOMevidence:::.fit_cross_partition(
    x, seq_len(nrow(x)), "gmm", 2L, 1103L,
    10L, 50L, NULL
  ))
  expect_identical(.Random.seed, rng_before)
})

test_that("resamples remain bound to ordered sample identities", {
  first <- simulate_som_scenario(
    "clusters", n = 36, p = 3, seed = 1110,
    id = paste0("first_", seq_len(36))
  )
  second <- som_data(
    layers = first$layers,
    id = paste0("second_", seq_len(36))
  )
  resamples <- som_resamples(first, method = "subsample", repeats = 2)
  specification <- som_spec(c(2, 2), seeds = 1L, rlen = 5L, k = 2L)

  expect_error(
    fit_som_ensemble(second, specification, resamples),
    "different sample IDs or row order"
  )
  expect_error(
    som_resamples(
      first,
      method = "custom",
      splits = list(
        list(id = "duplicate", analysis = 1:24),
        list(id = "duplicate", analysis = 13:36)
      )
    ),
    "unique `id`"
  )

  legacy <- resamples
  legacy$sample_ids <- NULL
  expect_error(
    fit_som_ensemble(first, specification, legacy),
    "no sample identity record"
  )
})

test_that("aligned voting propagates labels through a connected overlap chain", {
  n <- 12L
  record <- function(id, observed, labels) {
    output <- rep(NA_integer_, n)
    output[observed] <- labels
    list(id = id, sample_labels = output)
  }
  records <- list(
    record("one", 1:6, c(1, 1, 1, 2, 2, 2)),
    record("two", 3:8, c(1, 1, 2, 2, 1, 2)),
    record("three", 7:12, c(2, 1, 2, 2, 1, 1))
  )
  propagated <- SOMevidence:::.propagate_alignment(records, 1L, 2L)

  expect_true(all(!is.na(propagated$aligned[7:12, 3L])))
  expect_equal(nrow(propagated$diagnostics), 2L)
})

test_that("consensus coverage follows a four-partition overlap chain", {
  n <- 20L
  record <- function(id, observed) {
    labels <- rep(NA_integer_, n)
    labels[observed] <- rep(1:2, length.out = length(observed))
    list(id = id, k = 2L, sample_labels = labels)
  }
  records <- list(
    record("one", 1:8),
    record("two", 5:12),
    record("three", 9:16),
    record("four", 13:20)
  )
  partitions <- structure(
    list(
      records = records,
      scope = "analysis",
      ensemble = list(data = list(metadata = data.frame(
        id = paste0("sample_", seq_len(n))
      )))
    ),
    class = "som_partitions"
  )
  consensus <- consensus_som(partitions, k = 2L, method = "aligned_vote")

  expect_equal(consensus$consensus_label_coverage, 1)
  expect_true(all(!is.na(consensus$consensus_labels)))
  expect_equal(nrow(consensus$alignment_diagnostics), 3L)
})

test_that("incomplete sample-level k partitions cannot enter consensus", {
  labels <- c(rep(1L, 10), rep(NA_integer_, 2))
  partitions <- structure(
    list(
      records = list(
        list(id = "a", k = 2L, sample_labels = labels),
        list(id = "b", k = 2L, sample_labels = labels)
      )
    ),
    class = "som_partitions"
  )
  expect_error(
    consensus_som(partitions, k = 2L),
    "requires all k sample-level clusters"
  )
})

test_that("partition auditing enforces an explicit pairwise budget", {
  data <- simulate_som_scenario("clusters", n = 48, p = 3, seed = 1120)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(2, 2), seeds = 1:4, rlen = 5L, k = 2L),
    keep_models = FALSE
  )
  expect_error(
    partition_som(ensemble, k = 2L, max_pairwise_comparisons = 5L),
    "would create 6 pairwise comparisons"
  )
})

test_that("degenerate agreement metrics avoid sample-by-sample matrices", {
  constant <- rep.int(1L, 100000L)
  relabelled <- rep.int(9L, length(constant))

  expect_identical(
    SOMevidence:::.adjusted_rand(constant, relabelled),
    1
  )
  expect_identical(
    SOMevidence:::.adjusted_mutual_info(constant, relabelled),
    1
  )

  equivalent <- table(
    c(1L, 1L, 2L, 2L),
    c(8L, 8L, 7L, 7L)
  )
  crossed <- table(
    c(1L, 1L, 2L, 2L),
    c(7L, 8L, 7L, 8L)
  )
  expect_true(SOMevidence:::.same_partition_from_table(equivalent))
  expect_false(SOMevidence:::.same_partition_from_table(crossed))

  unused_x <- factor(rep("a", 4L), levels = c("a", "unused_x"))
  unused_y <- factor(rep("z", 4L), levels = c("z", "unused_y"))
  expect_identical(SOMevidence:::.adjusted_rand(unused_x, unused_y), 1)
  expect_identical(
    SOMevidence:::.adjusted_mutual_info(unused_x, unused_y),
    1
  )
  expect_true(SOMevidence:::.same_partition_from_table(table(
    unused_x, unused_y
  )))
})

test_that("finite integer validation rejects truncation and overflow", {
  expect_error(som_spec(c(2, 2), rlen = 2.5), "integer")
  expect_error(som_spec(c(2, 2), cores = Inf, k = 2L), "one number")
  expect_error(som_spec(c(2, 2), seeds = 2^31), "integers")

  data <- simulate_som_scenario("clusters", n = 36, p = 3, seed = 1130)
  expect_error(
    som_resamples(data, method = "subsample", repeats = 2.5),
    "integer"
  )
})

test_that("training-constant variables are excluded from held-out distance", {
  training <- cbind(constant = rep(5, 8), varying = seq_len(8))
  fitted <- SOMevidence:::.fit_preprocessor(training, som_preprocess())$fitted
  held_out <- cbind(constant = c(5, 500), varying = c(2, 7))
  transformed <- SOMevidence:::.apply_preprocessor(held_out, fitted)

  expect_true(fitted$constant[[1L]])
  expect_identical(transformed[, "constant"], c(0, 0))
})

test_that("consensus collapse and incomplete evidence cannot pass gates", {
  metadata <- data.frame(id = paste0("sample_", 1:10))
  ensemble <- list(data = list(metadata = metadata))
  minority_sets <- list(c(1L, 2L), c(2L, 3L), c(3L, 4L), c(4L, 5L), c(5L, 1L))
  records <- lapply(seq_along(minority_sets), function(i) {
    labels <- rep(1L, 10L)
    labels[minority_sets[[i]]] <- 2L
    list(
      id = paste0("fit_", i), k = 2L, sample_labels = labels,
      complete_k = TRUE
    )
  })
  stability <- data.frame(
    k = 2L, median_ari = 0, median_ami = 0,
    median_joint_coverage = 1, n_complete_partitions = 5L,
    n_partitions = 5L
  )
  partitions <- structure(
    list(
      records = records, stability = stability, scope = "analysis",
      partition_method = "ward.D2",
      ensemble = ensemble
    ),
    class = "som_partitions"
  )
  consensus <- consensus_som(
    partitions, k = 2L, method = "aligned_vote"
  )
  expect_identical(consensus$n_consensus_clusters, 1L)
  expect_false(consensus$complete_consensus_k)

  audit <- structure(
    list(
      fit_metrics = data.frame(
        topographic_error = 0, empty_unit_rate = 0
      ),
      success_rate = 1,
      ensemble = ensemble
    ),
    class = "som_audit"
  )
  decision <- assess_defensibility(
    audit, partitions, k = 2L,
    gate = som_gate(min_success_rate = 0),
    consensus = consensus
  )
  expect_identical(decision$status, "abstain")
  expect_false(decision$checks$passed[
    decision$checks$requirement == "consensus_observes_k"
  ])

  comparison <- structure(
    list(
      comparisons = data.frame(k = 2L, method = "ward", ari = 1),
      methods = "ward",
      reference_status = data.frame(
        method = "ward", k = 2L, success_rate = 0.5
      ),
      partition_completeness = data.frame(
        k = 2L, n_partitions = 2L, n_complete_partitions = 1L
      ),
      partition_records = partitions$records,
      partition_method = partitions$partition_method,
      scope = "analysis",
      ensemble = ensemble
    ),
    class = "som_cross_comparison"
  )
  cross_decision <- assess_defensibility(
    audit, k = 2L,
    gate = som_gate(min_cross_model_ari = 0),
    cross_model = comparison
  )
  expect_identical(cross_decision$status, "abstain")
  expect_error(
    assess_defensibility(
      audit, k = c(2, 3), gate = som_gate(min_success_rate = 0)
    ),
    "one number"
  )
})
