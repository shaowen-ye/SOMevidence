.external_consensus_fixture <- function() {
  ids <- paste0("sample_", seq_len(10L))
  consensus_labels <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
  external_labels <- c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c")
  metadata <- data.frame(
    id = ids,
    external_label = external_labels,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      consensus_labels = consensus_labels,
      assignment_count = rep(2L, length(ids)),
      aligned_labels = cbind(first = consensus_labels, second = consensus_labels),
      coassignment = NULL,
      sample_ids = ids,
      metadata = metadata,
      ensemble = list(
        data = list(metadata = metadata),
        resamples = list(sample_ids = ids)
      )
    ),
    class = "som_consensus"
  )
}

test_that("external labels remain a post hoc agreement assessment", {
  d <- simulate_som_scenario("clusters", n = 75, p = 4, seed = 801)
  e <- fit_som_ensemble(
    d, som_spec(c(3, 2), seeds = c(802, 803), rlen = 15, k = 3)
  )
  consensus <- consensus_som(partition_som(e), k = 3)
  assessment <- evaluate_external_labels(consensus)

  expect_s3_class(assessment, "som_external_assessment")
  expect_identical(assessment$match_method, "stored")
  expect_equal(assessment$n_used, nrow(d$metadata))
  expect_true(assessment$ari >= -1 && assessment$ari <= 1)
  expect_true(assessment$ami >= -1 && assessment$ami <= 1)
  expect_equal(sum(assessment$contingency), nrow(d$metadata))
  expect_identical(
    assessment$sample_accounting$sample_id,
    consensus$sample_ids
  )
  expect_identical(
    assessment$n_used + assessment$n_omitted,
    assessment$n_total
  )
  expect_identical(sum(assessment$omission_counts$n), assessment$n_omitted)
})

test_that("ID matching is invariant to supplied label order", {
  consensus <- .external_consensus_fixture()
  ids <- consensus$sample_ids
  labels <- stats::setNames(consensus$metadata$external_label, ids)
  baseline <- evaluate_external_labels(consensus)
  shuffled <- evaluate_external_labels(consensus, labels = labels[rev(ids)])
  explicit <- evaluate_external_labels(
    consensus,
    labels = unname(labels[rev(ids)]),
    label_ids = rev(ids)
  )

  for (assessment in list(shuffled, explicit)) {
    expect_identical(assessment$ari, baseline$ari)
    expect_identical(assessment$ami, baseline$ami)
    expect_identical(assessment$contingency, baseline$contingency)
    expect_identical(assessment$n_used, baseline$n_used)
    expect_true(assessment$matching$input_reordered)
    expect_identical(assessment$match_method, "id")
  }
})

test_that("ID matching permits audited subsets and records all omissions", {
  consensus <- .external_consensus_fixture()
  consensus$consensus_labels[[4L]] <- NA_integer_
  consensus$assignment_count[[5L]] <- 1L
  consensus$aligned_labels[5L, 2L] <- NA_integer_
  ids <- consensus$sample_ids[-1L]
  labels <- consensus$metadata$external_label[-1L]
  labels[[1L]] <- NA_character_
  labels[[2L]] <- "unknown"

  assessment <- evaluate_external_labels(
    consensus,
    labels = labels,
    label_ids = ids,
    exclude = "unknown"
  )

  expected_status <- c(
    "label_id_absent",
    "external_label_missing",
    "external_label_excluded",
    "consensus_label_missing",
    "insufficient_consensus_replication",
    rep("used", 5L)
  )
  expect_identical(assessment$sample_accounting$status, expected_status)
  expect_identical(assessment$n_total, 10L)
  expect_identical(assessment$n_used, 5L)
  expect_identical(assessment$n_omitted, 5L)
  expect_identical(assessment$omission_counts$n, rep(1L, 5L))
  expect_identical(sum(assessment$contingency), assessment$n_used)
  expect_identical(assessment$matching$n_input, 9L)
  expect_identical(assessment$matching$n_matched, 9L)
  expect_identical(assessment$matching$n_consensus_without_input, 1L)
  expect_identical(assessment$matching$n_unmatched_input, 0L)
})

test_that("positional matching is explicit and legacy auto matching warns", {
  consensus <- .external_consensus_fixture()
  labels <- consensus$metadata$external_label
  explicit <- evaluate_external_labels(
    consensus, labels = labels, match_by = "position"
  )
  expect_warning(
    automatic <- evaluate_external_labels(consensus, labels = labels),
    "legacy positional matching",
    fixed = TRUE
  )

  expect_identical(automatic$contingency, explicit$contingency)
  expect_identical(automatic$match_method, "position")
  expect_identical(explicit$matching$requested, "position")
  expect_false(explicit$matching$input_reordered)
})

test_that("unsafe or ambiguous external-label identities are rejected", {
  consensus <- .external_consensus_fixture()
  ids <- consensus$sample_ids
  labels <- consensus$metadata$external_label

  expect_error(
    evaluate_external_labels(consensus, labels = labels[-1L]),
    "cannot be aligned without sample IDs",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(
      consensus, labels = labels, match_by = "id"
    ),
    "requires `label_ids`",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(
      consensus, labels = labels, label_ids = c(ids[-1L], "outside")
    ),
    "absent from the consensus",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(
      consensus, labels = labels, label_ids = c(ids[-1L], ids[[2L]])
    ),
    "unique, non-empty",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(
      consensus,
      labels = stats::setNames(labels, c(ids[-1L], ""))
    ),
    "unique, non-empty",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(
      consensus,
      labels = stats::setNames(labels, ids),
      label_ids = rev(ids)
    ),
    "same rows in the same order",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(
      consensus,
      labels = stats::setNames(labels, rev(ids)),
      match_by = "position"
    ),
    "do not match consensus row order",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(consensus, label_ids = ids),
    "cannot be supplied when `labels` is NULL",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(consensus, match_by = "position"),
    "applies only when `labels` are supplied explicitly",
    fixed = TRUE
  )
  expect_error(
    evaluate_external_labels(consensus, labels = as.list(labels)),
    "atomic vector",
    fixed = TRUE
  )
})

test_that("consensus identity corruption is rejected before assessment", {
  consensus <- .external_consensus_fixture()

  reordered <- consensus
  reordered$metadata <- reordered$metadata[rev(seq_len(10L)), , drop = FALSE]
  expect_error(
    evaluate_external_labels(reordered),
    "sample_ids` and `consensus$metadata$id` disagree",
    fixed = TRUE
  )

  wrong_origin <- consensus
  wrong_origin$ensemble$data$metadata$id[[1L]] <- "different"
  expect_error(
    evaluate_external_labels(wrong_origin),
    "do not match the originating ensemble",
    fixed = TRUE
  )

  wrong_resamples <- consensus
  wrong_resamples$ensemble$resamples$sample_ids <- rev(consensus$sample_ids)
  expect_error(
    evaluate_external_labels(wrong_resamples),
    "do not match the originating resamples",
    fixed = TRUE
  )

  nonfinite_count <- consensus
  nonfinite_count$assignment_count[[1L]] <- Inf
  expect_error(
    evaluate_external_labels(nonfinite_count),
    "assignment_count` is invalid",
    fixed = TRUE
  )

  fractional_count <- consensus
  fractional_count$assignment_count[[1L]] <- 1.5
  expect_error(
    evaluate_external_labels(fractional_count),
    "assignment_count` is invalid",
    fixed = TRUE
  )

  excessive_count <- consensus
  excessive_count$assignment_count[[1L]] <- 3L
  excessive_count$aligned_labels <-
    excessive_count$aligned_labels[, 1L, drop = FALSE]
  expect_error(
    evaluate_external_labels(excessive_count),
    "disagrees with the available aligned partitions",
    fixed = TRUE
  )

  absent_alignment <- consensus
  absent_alignment$aligned_labels <- NULL
  expect_error(
    evaluate_external_labels(absent_alignment),
    "not sample-aligned",
    fixed = TRUE
  )

  wrong_alignment <- consensus
  wrong_alignment$aligned_labels <- wrong_alignment$aligned_labels[-1L, ]
  expect_error(
    evaluate_external_labels(wrong_alignment),
    "not sample-aligned",
    fixed = TRUE
  )
})

test_that("legacy consensus objects retain stored-row semantics", {
  consensus <- .external_consensus_fixture()
  current <- evaluate_external_labels(consensus)
  legacy <- consensus
  legacy$sample_ids <- NULL

  assessment <- evaluate_external_labels(legacy)
  expect_identical(assessment$contingency, current$contingency)
  expect_identical(assessment$ari, current$ari)
  expect_identical(assessment$ami, current$ami)
  expect_identical(assessment$match_method, "stored")
  expect_identical(
    assessment$matching$identifier_source,
    "legacy stored metadata"
  )
})

test_that("insufficient external evidence reports usable and omitted counts", {
  consensus <- .external_consensus_fixture()
  labels <- rep("one", 10L)
  labels[[1L]] <- NA_character_
  expect_error(
    evaluate_external_labels(
      consensus, labels = labels, match_by = "position"
    ),
    "n_used=9; omissions: label_id_absent=0, external_label_missing=1",
    fixed = TRUE
  )
})

test_that("external assessment excludes samples without consensus support", {
  consensus <- .external_consensus_fixture()
  consensus$consensus_labels[1:3] <- NA_integer_
  assessment <- evaluate_external_labels(consensus)

  expect_equal(assessment$n_used, 7L)
  expect_equal(assessment$n_omitted, 3L)
  expect_equal(sum(assessment$contingency), 7L)
  expect_identical(
    assessment$omission_counts$n[
      assessment$omission_counts$status == "consensus_label_missing"
    ],
    3L
  )
})

test_that("external labels require replicated consensus assignments", {
  consensus <- .external_consensus_fixture()
  consensus$assignment_count[1:2] <- 1L
  consensus$aligned_labels[1:2, 2L] <- NA_integer_
  assessment <- evaluate_external_labels(consensus)

  expect_equal(assessment$n_used, 8L)
  expect_equal(sum(assessment$contingency), 8L)
  expect_identical(
    assessment$omission_counts$n[
      assessment$omission_counts$status ==
        "insufficient_consensus_replication"
    ],
    2L
  )
})
