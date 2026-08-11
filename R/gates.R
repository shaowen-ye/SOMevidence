#' Define explicit evidence gates for a hard SOM partition
#'
#' No universal thresholds are supplied. A gate records the analyst's
#' prespecified requirements.
#'
#' @param max_topographic_error Optional maximum median topographic error.
#' @param max_empty_unit_rate Optional maximum median empty-unit rate.
#' @param min_median_ari Optional minimum median pairwise ARI.
#' @param min_median_ami Optional minimum median pairwise AMI.
#' @param min_pairwise_coverage Optional minimum median proportion of samples
#'   jointly assigned in pairwise partition comparisons.
#' @param min_cluster_jaccard Optional minimum across cluster-level median
#'   Jaccard values.
#' @param min_consensus_coverage Optional minimum proportion of samples with an
#'   identifiable consensus label.
#' @param min_replicated_coverage Optional minimum proportion of
#'   samples assigned by at least two partitions, for which support and entropy
#'   are estimable.
#' @param min_membership_support Optional minimum median sample membership
#'   support.
#' @param max_assignment_entropy Optional maximum median normalized assignment
#'   entropy.
#' @param min_cross_model_ari Optional minimum of the method-specific median
#'   SOM-to-reference ARI values. Each requested reference method must have an
#'   evaluable result; pooled comparisons are not used for this gate.
#' @param min_cross_model_methods Optional minimum number of reference methods
#'   represented in the comparison.
#' @param min_success_rate Optional minimum model-fit success rate.
#'
#' @return A `som_gate` object.
#' @examples
#' som_gate(min_success_rate = 0.95, min_cluster_jaccard = 0.75)
#' @export
som_gate <- function(max_topographic_error = NULL,
                     max_empty_unit_rate = NULL,
                     min_median_ari = NULL,
                     min_median_ami = NULL,
                     min_pairwise_coverage = NULL,
                     min_cluster_jaccard = NULL,
                     min_consensus_coverage = NULL,
                     min_replicated_coverage = NULL,
                     min_membership_support = NULL,
                     max_assignment_entropy = NULL,
                     min_cross_model_ari = NULL,
                     min_cross_model_methods = NULL,
                     min_success_rate = NULL) {
  values <- list(
    max_topographic_error = max_topographic_error,
    max_empty_unit_rate = max_empty_unit_rate,
    min_median_ari = min_median_ari,
    min_median_ami = min_median_ami,
    min_pairwise_coverage = min_pairwise_coverage,
    min_cluster_jaccard = min_cluster_jaccard,
    min_consensus_coverage = min_consensus_coverage,
    min_replicated_coverage = min_replicated_coverage,
    min_membership_support = min_membership_support,
    max_assignment_entropy = max_assignment_entropy,
    min_cross_model_ari = min_cross_model_ari,
    min_cross_model_methods = min_cross_model_methods,
    min_success_rate = min_success_rate
  )
  if (all(vapply(values, is.null, logical(1)))) {
    .abort("Define at least one evidence requirement; no universal gate is assumed.")
  }
  for (nm in setdiff(names(values), "min_cross_model_methods")) {
    if (!is.null(values[[nm]])) {
      .assert_scalar_number(values[[nm]], nm, 0, 1)
    }
  }
  if (!is.null(min_cross_model_methods)) {
    .assert_scalar_number(
      min_cross_model_methods, "min_cross_model_methods",
      lower = 1
    )
    if (min_cross_model_methods %% 1 != 0) {
      .abort("`min_cross_model_methods` must be an integer.")
    }
  }
  structure(values, class = "som_gate")
}

#' @export
print.som_gate <- function(x, ...) {
  cat("<som_gate> (analyst-specified)\n")
  active <- x[!vapply(x, is.null, logical(1))]
  for (nm in names(active)) cat("  ", nm, ":", active[[nm]], "\n")
  invisible(x)
}

#' Assess partition defensibility without forcing a verdict
#'
#' In addition to analyst-defined thresholds, the assessment checks that every
#' source partition and the consensus retain the requested `k`, and that all
#' requested cross-model fits succeeded. Missing audit evidence produces an
#' uncertain result rather than being treated as a pass.
#'
#' @param audit A `som_audit` object.
#' @param partitions Optional `som_partitions` object.
#' @param k Candidate number of clusters when partition stability is assessed.
#' @param gate Optional analyst-defined `som_gate`. Without one the status is
#'   `"not_assessed"`.
#' @param consensus Optional `som_consensus` object for the same `k`.
#' @param cross_model Optional `som_cross_comparison` object.
#'
#' @return A `som_defensibility` object containing evidence and reasons.
#' @export
assess_defensibility <- function(audit, partitions = NULL, k = NULL, gate = NULL,
                                 consensus = NULL, cross_model = NULL) {
  if (!inherits(audit, "som_audit")) .abort("`audit` must come from `audit_som()`.")
  if (!is.null(k)) {
    .assert_scalar_integer(k, "k", lower = 2)
    k <- as.integer(k)
  }
  if (!is.null(partitions) && !inherits(partitions, "som_partitions")) {
    .abort("`partitions` must come from `partition_som()`.")
  }
  if (!is.null(partitions) && !identical(partitions$scope, "analysis")) {
    .abort("Defensibility gates require analysis-scoped SOM partitions.")
  }
  if (!is.null(partitions) &&
        !identical(audit$ensemble, partitions$ensemble)) {
    .abort("All defensibility evidence must share the same source ensemble.")
  }
  if (!is.null(gate) && !inherits(gate, "som_gate")) {
    .abort("`gate` must come from `som_gate()`.")
  }
  if (!is.null(consensus) && !inherits(consensus, "som_consensus")) {
    .abort("`consensus` must come from `consensus_som()`.")
  }
  if (!is.null(consensus) && !identical(consensus$scope, "analysis")) {
    .abort("Defensibility gates require analysis-scoped SOM consensus.")
  }
  if (!is.null(consensus) &&
        !identical(audit$ensemble, consensus$ensemble)) {
    .abort("All defensibility evidence must share the same source ensemble.")
  }
  if (!is.null(cross_model) &&
        !inherits(cross_model, "som_cross_comparison")) {
    .abort("`cross_model` must come from `compare_cross_models()`.")
  }
  if (!is.null(cross_model) && !identical(cross_model$scope, "analysis")) {
    .abort("Defensibility gates require analysis-scoped cross-model evidence.")
  }
  if (!is.null(cross_model) &&
        !identical(audit$ensemble, cross_model$ensemble)) {
    .abort("All defensibility evidence must share the same source ensemble.")
  }

  median_ari <- NA_real_
  median_ami <- NA_real_
  median_pairwise_coverage <- NA_real_
  complete_partition_rate <- NA_real_
  if (!is.null(partitions)) {
    if (is.null(k)) .abort("Supply `k` when partition evidence is included.")
    row <- partitions$stability[partitions$stability$k == k, , drop = FALSE]
    if (nrow(row) != 1L) {
      .abort("The requested `k` was not evaluated in `partitions`.")
    }
    median_ari <- row$median_ari
    median_ami <- row$median_ami
    median_pairwise_coverage <- row$median_joint_coverage
    if (all(c(
      "n_complete_partitions", "n_partitions"
    ) %in% names(row))) {
      complete_partition_rate <-
        row$n_complete_partitions / row$n_partitions
    } else {
      candidate_records <- Filter(function(record) {
        record$k == k
      }, partitions$records)
      if (length(candidate_records)) {
        complete_partition_rate <- mean(vapply(
          candidate_records,
          function(record) {
            length(unique(stats::na.omit(record$sample_labels))) == k
          },
          logical(1)
        ))
      }
    }
  } else if (!is.null(cross_model) &&
               !is.null(cross_model$partition_completeness)) {
    row <- cross_model$partition_completeness[
      cross_model$partition_completeness$k == k, , drop = FALSE
    ]
    if (nrow(row) == 1L && all(c(
      "n_complete_partitions", "n_partitions"
    ) %in% names(row))) {
      complete_partition_rate <-
        row$n_complete_partitions / row$n_partitions
    }
  } else if (!is.null(consensus)) {
    record_complete <- vapply(consensus$records, function(record) {
      complete <- record$complete_k
      if (is.null(complete)) {
        complete <- length(unique(stats::na.omit(record$sample_labels))) == k
      }
      isTRUE(complete)
    }, logical(1))
    if (length(record_complete)) {
      complete_partition_rate <- mean(record_complete)
    }
  }
  min_cluster_jaccard <- median_membership_support <-
    median_assignment_entropy <- NA_real_
  consensus_assignment_coverage <- replicated_assignment_coverage <- NA_real_
  consensus_cluster_count <- NA_real_
  if (!is.null(consensus)) {
    if (is.null(k) || consensus$k != k) {
      .abort("`consensus` and the requested `k` must agree.")
    }
    cluster_jaccard <- consensus$cluster_summary$median_jaccard
    min_cluster_jaccard <- if (
      length(cluster_jaccard) && all(is.finite(cluster_jaccard))
    ) {
      min(cluster_jaccard)
    } else {
      NA_real_
    }
    median_membership_support <- stats::median(
      consensus$membership_support,
      na.rm = TRUE
    )
    median_assignment_entropy <- stats::median(
      consensus$assignment_entropy,
      na.rm = TRUE
    )
    consensus_assignment_coverage <- consensus$consensus_label_coverage %||%
      consensus$assignment_coverage
    consensus_cluster_count <- consensus$n_consensus_clusters %||%
      length(unique(stats::na.omit(consensus$consensus_labels)))
    replicated_assignment_coverage <-
      consensus$replicated_assignment_coverage
  }
  cross_model_median_ari <- cross_model_pooled_median_ari <-
    cross_model_methods <- cross_model_min_success_rate <- NA_real_
  if (!is.null(cross_model)) {
    if (is.null(k)) .abort("Supply `k` when cross-model evidence is included.")
    cross_rows <- cross_model$comparisons[
      cross_model$comparisons$k == k, ,
      drop = FALSE
    ]
    if (nrow(cross_rows)) {
      finite_rows <- cross_rows[is.finite(cross_rows$ari), , drop = FALSE]
      cross_model_pooled_median_ari <- if (nrow(finite_rows)) {
        stats::median(finite_rows$ari)
      } else {
        NA_real_
      }
      method_medians <- vapply(
        cross_model$methods %||% unique(cross_rows$method),
        function(method) {
          values <- finite_rows$ari[finite_rows$method == method]
          if (length(values)) stats::median(values) else NA_real_
        },
        numeric(1)
      )
      cross_model_median_ari <- if (
        length(method_medians) && all(is.finite(method_medians))
      ) {
        min(method_medians)
      } else {
        NA_real_
      }
      cross_model_methods <- sum(is.finite(method_medians))
    }
    if (!is.null(cross_model$reference_status)) {
      status_rows <- cross_model$reference_status[
        cross_model$reference_status$k == k, , drop = FALSE
      ]
      method_rates <- vapply(
        cross_model$methods,
        function(method) {
          values <- status_rows$success_rate[status_rows$method == method]
          if (length(values) == 1L) values else NA_real_
        },
        numeric(1)
      )
      if (length(method_rates) && all(is.finite(method_rates))) {
        cross_model_min_success_rate <- min(method_rates)
      }
    }
  }
  evidence <- c(
    median_topographic_error = stats::median(
      audit$fit_metrics$topographic_error,
      na.rm = TRUE
    ),
    median_empty_unit_rate = stats::median(
      audit$fit_metrics$empty_unit_rate,
      na.rm = TRUE
    ),
    median_ari = median_ari,
    median_ami = median_ami,
    median_pairwise_coverage = median_pairwise_coverage,
    complete_partition_rate = complete_partition_rate,
    min_cluster_jaccard = min_cluster_jaccard,
    consensus_assignment_coverage = consensus_assignment_coverage,
    consensus_cluster_count = consensus_cluster_count,
    replicated_assignment_coverage = replicated_assignment_coverage,
    median_membership_support = median_membership_support,
    median_assignment_entropy = median_assignment_entropy,
    cross_model_median_ari = cross_model_median_ari,
    cross_model_pooled_median_ari = cross_model_pooled_median_ari,
    cross_model_methods = cross_model_methods,
    cross_model_min_success_rate = cross_model_min_success_rate,
    success_rate = audit$success_rate
  )

  if (is.null(gate)) {
    checks <- data.frame(
      requirement = character(), observed = numeric(), threshold = numeric(),
      passed = logical(), stringsAsFactors = FALSE
    )
    status <- "not_assessed"
  } else {
    rules <- list(
      max_topographic_error = c("median_topographic_error", "max"),
      max_empty_unit_rate = c("median_empty_unit_rate", "max"),
      min_median_ari = c("median_ari", "min"),
      min_median_ami = c("median_ami", "min"),
      min_pairwise_coverage = c("median_pairwise_coverage", "min"),
      min_cluster_jaccard = c("min_cluster_jaccard", "min"),
      min_consensus_coverage = c(
        "consensus_assignment_coverage", "min"
      ),
      min_replicated_coverage = c(
        "replicated_assignment_coverage", "min"
      ),
      min_membership_support = c("median_membership_support", "min"),
      max_assignment_entropy = c("median_assignment_entropy", "max"),
      min_cross_model_ari = c("cross_model_median_ari", "min"),
      min_cross_model_methods = c("cross_model_methods", "min"),
      min_success_rate = c("success_rate", "min")
    )
    active <- names(gate)[!vapply(gate, is.null, logical(1))]
    checks <- do.call(rbind, lapply(active, function(nm) {
      observed <- evidence[[rules[[nm]][[1L]]]]
      threshold <- gate[[nm]]
      passed <- if (!is.finite(observed)) {
        NA
      } else if (rules[[nm]][[2L]] == "max") {
        observed <= threshold
      } else {
        observed >= threshold
      }
      data.frame(
        requirement = nm, observed = observed, threshold = threshold,
        passed = passed, stringsAsFactors = FALSE
      )
    }))
    has_partition_evidence <- !is.null(partitions) || !is.null(consensus) ||
      !is.null(cross_model)
    if (has_partition_evidence) {
      checks <- rbind(
        data.frame(
          requirement = "all_partitions_observe_k",
          observed = complete_partition_rate,
          threshold = 1,
          passed = if (is.finite(complete_partition_rate)) {
            complete_partition_rate == 1
          } else {
            NA
          },
          stringsAsFactors = FALSE
        ),
        checks
      )
    }
    if (!is.null(consensus)) {
      checks <- rbind(
        data.frame(
          requirement = "consensus_observes_k",
          observed = consensus_cluster_count,
          threshold = k,
          passed = if (is.finite(consensus_cluster_count)) {
            consensus_cluster_count == k
          } else {
            NA
          },
          stringsAsFactors = FALSE
        ),
        checks
      )
    }
    if (!is.null(cross_model)) {
      checks <- rbind(
        data.frame(
          requirement = "all_cross_model_fits_succeeded",
          observed = cross_model_min_success_rate,
          threshold = 1,
          passed = if (is.finite(cross_model_min_success_rate)) {
            cross_model_min_success_rate == 1
          } else {
            NA
          },
          stringsAsFactors = FALSE
        ),
        checks
      )
    }
    status <- if (any(checks$passed %in% FALSE)) {
      "abstain"
    } else if (anyNA(checks$passed)) {
      "uncertain"
    } else {
      "supported"
    }
  }

  structure(
    list(status = status, k = k, evidence = evidence, checks = checks),
    class = "som_defensibility"
  )
}

#' @export
print.som_defensibility <- function(x, ...) {
  cat("<som_defensibility>\n")
  cat("  status:", x$status, "\n")
  if (!is.null(x$k)) cat("  k     :", x$k, "\n")
  if (nrow(x$checks)) {
    for (i in seq_len(nrow(x$checks))) {
      result <- if (is.na(x$checks$passed[[i]])) {
        "unavailable"
      } else if (x$checks$passed[[i]]) {
        "pass"
      } else {
        "fail"
      }
      cat(sprintf(
        "  - %s: observed=%s, threshold=%s, %s\n",
        x$checks$requirement[[i]],
        format(x$checks$observed[[i]], digits = 3),
        format(x$checks$threshold[[i]], digits = 3),
        result
      ))
    }
  }
  invisible(x)
}
