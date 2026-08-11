#' Run a complete SOM defensibility workflow
#'
#' The workflow preserves representation quality, partition stability,
#' sample-level consensus and cross-model agreement as separate evidence
#' streams. It does not combine them into a score or select a winning `k`.
#'
#' @param data A `som_data` object.
#' @param spec A `som_spec` object.
#' @param resamples Optional `som_resamples` object. If `NULL`, a single
#'   full-data split is used; that run does not assess perturbation under
#'   resampling.
#' @param preprocess One `som_preprocess` object or a named object per layer.
#' @param k Candidate numbers of hard partitions.
#' @param cross_models Controlled reference methods passed to
#'   [fit_cross_models()]. Use `character()` to skip cross-model fitting.
#' @param cross_model_control Named list of optional `kmeans_seeds`,
#'   `kmeans_nstart`, `kmeans_iter_max`, `gmm_model_names` or `gmm_seed`
#'   settings forwarded
#'   to [fit_cross_models()]. Methods, candidate `k`, model retention and
#'   failure policy remain controlled by this workflow.
#' @param consensus_method Consensus method passed to [consensus_som()].
#' @param max_coassignment_n Dense co-assignment limit passed to
#'   [consensus_som()].
#' @param keep_models Whether to retain fitted SOM and reference model objects.
#' @param fail_fast Whether a model or consensus failure should stop the run.
#' @param parallel Whether SOM ensemble members should use the current
#'   `future` plan; passed to [fit_som_ensemble()].
#' @param max_pairwise_comparisons Pairwise partition-comparison budget passed
#'   to [partition_som()].
#'
#' @return A `som_workflow` object containing each evidence stream and explicit
#'   failure logs.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 6)
#' specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2:3)
#' workflow <- run_som_workflow(
#'   data, specification, cross_models = c("kmeans", "ward")
#' )
#' summary(workflow)
#' @export
run_som_workflow <- function(
  data,
  spec,
  resamples = NULL,
  preprocess = som_preprocess(),
  k = spec$k,
  cross_models = c("kmeans", "ward"),
  cross_model_control = list(),
  consensus_method = "auto",
  max_coassignment_n = 5000L,
  keep_models = FALSE,
  fail_fast = FALSE,
  parallel = FALSE,
  max_pairwise_comparisons = 1000000L
) {
  .assert_flag(keep_models, "keep_models")
  .assert_flag(fail_fast, "fail_fast")
  allowed_control <- c(
    "kmeans_seeds", "kmeans_nstart", "kmeans_iter_max", "gmm_model_names",
    "gmm_seed"
  )
  invalid_control_names <- is.list(cross_model_control) &&
    length(cross_model_control) &&
    (is.null(names(cross_model_control)) ||
       any(names(cross_model_control) == "") ||
       anyDuplicated(names(cross_model_control)) ||
       any(!names(cross_model_control) %in% allowed_control))
  if (!is.list(cross_model_control) || invalid_control_names) {
    .abort(paste0(
      "`cross_model_control` must be a uniquely named list containing only: ",
      paste(allowed_control, collapse = ", "), "."
    ))
  }
  if (!inherits(data, "som_data")) {
    .abort("`data` must come from `som_data()`.")
  }
  if (!inherits(spec, "som_spec")) {
    .abort("`spec` must come from `som_spec()`.")
  }
  .assert_integer_vector(k, "k", lower = 2)
  .assert_scalar_integer(
    max_pairwise_comparisons, "max_pairwise_comparisons", lower = 1
  )
  resamples <- resamples %||% som_resamples(data, method = "full")
  if (!inherits(resamples, "som_resamples") ||
        resamples$n != nrow(data$metadata)) {
    .abort("`resamples` must describe the rows in `data`.")
  }
  if (is.null(resamples$sample_ids)) {
    .abort(paste0(
      "This legacy `som_resamples` object has no sample identity record. ",
      "Recreate it with `som_resamples()` before fitting."
    ))
  }
  if (!identical(
    as.character(resamples$sample_ids),
    as.character(data$metadata$id)
  )) {
    .abort(paste0(
      "`resamples` were created for different sample IDs or row order. ",
      "Recreate them from the current `som_data` object."
    ))
  }
  planned_models <- as.double(length(resamples$splits)) *
    nrow(expand_som_spec(spec))
  planned_pairwise <- choose(planned_models, 2) *
    length(unique(as.integer(k)))
  if (!is.finite(planned_pairwise) ||
        planned_pairwise > max_pairwise_comparisons) {
    .abort(paste0(
      "The planned workflow would create ", format(planned_pairwise),
      " pairwise comparisons before any SOM fitting, exceeding ",
      "`max_pairwise_comparisons = ",
      format(max_pairwise_comparisons), "`. Reduce the model budget or ",
      "increase the limit deliberately."
    ))
  }
  ensemble <- fit_som_ensemble(
    data = data,
    spec = spec,
    resamples = resamples,
    preprocess = preprocess,
    keep_models = keep_models,
    fail_fast = fail_fast,
    parallel = parallel
  )
  audit <- audit_som(ensemble)
  partitions <- partition_som(
    ensemble, k = k,
    max_pairwise_comparisons = max_pairwise_comparisons
  )

  consensus <- list()
  consensus_failures <- list()
  for (candidate_k in sort(unique(as.integer(k)))) {
    result <- tryCatch(
      consensus_som(
        partitions,
        k = candidate_k,
        method = consensus_method,
        max_coassignment_n = max_coassignment_n
      ),
      error = function(e) {
        if (fail_fast) stop(e)
        e
      }
    )
    key <- paste0("k", candidate_k)
    if (inherits(result, "error")) {
      consensus_failures[[length(consensus_failures) + 1L]] <- data.frame(
        k = candidate_k,
        error = conditionMessage(result),
        stringsAsFactors = FALSE
      )
    } else {
      consensus[[key]] <- result
    }
  }
  consensus_failure_table <- if (length(consensus_failures)) {
    do.call(rbind, consensus_failures)
  } else {
    data.frame(k = integer(), error = character(), stringsAsFactors = FALSE)
  }

  references <- comparison <- NULL
  if (length(cross_models)) {
    references <- do.call(
      fit_cross_models,
      c(
        list(
          ensemble = ensemble,
          methods = cross_models,
          k = k,
          keep_models = keep_models,
          fail_fast = fail_fast
        ),
        cross_model_control
      )
    )
    comparison <- compare_cross_models(partitions, references)
  }

  provenance_dependencies <- c("kohonen", "clue", "withr")
  if (!is.null(references) && "gmm" %in% references$methods) {
    provenance_dependencies <- c(provenance_dependencies, "mclust")
  }
  dependency_versions <- vapply(provenance_dependencies, function(package) {
    if (requireNamespace(package, quietly = TRUE)) {
      as.character(utils::packageVersion(package))
    } else {
      NA_character_
    }
  }, character(1))
  output <- structure(
    list(
      ensemble = ensemble,
      audit = audit,
      partitions = partitions,
      consensus = consensus,
      consensus_failures = consensus_failure_table,
      cross_models = references,
      cross_comparison = comparison,
      requested_k = sort(unique(as.integer(k))),
      provenance = list(
        package_version = tryCatch(
          as.character(utils::packageVersion("SOMevidence")),
          error = function(e) NA_character_
        ),
        r_version = R.version.string,
        platform = R.version$platform,
        dependency_versions = dependency_versions,
        sample_ids = as.character(data$metadata$id),
        split_ids = vapply(
          ensemble$resamples$splits, `[[`, character(1), "id"
        )
      )
    ),
    class = "som_workflow"
  )
  output
}

.workflow_consensus_summary <- function(object, requested_k) {
  consensus_objects <- object$consensus %||% list()
  do.call(rbind, lapply(requested_k, function(candidate_k) {
    consensus <- consensus_objects[[paste0("k", candidate_k)]]
    if (is.null(consensus)) {
      return(data.frame(
        k = as.integer(candidate_k), status = "failed",
        computation_status = "not_computed",
        n_consensus_clusters = NA_integer_,
        complete_consensus_k = NA,
        assignment_coverage = NA_real_,
        consensus_label_coverage = NA_real_,
        replicated_assignment_coverage = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    labels <- consensus$consensus_labels
    assignment_count <- consensus$assignment_count
    n_clusters <- consensus$n_consensus_clusters
    if (is.null(n_clusters) && !is.null(labels)) {
      n_clusters <- length(unique(stats::na.omit(labels)))
    }
    n_clusters <- if (length(n_clusters) == 1L && !is.na(n_clusters)) {
      as.integer(n_clusters)
    } else {
      NA_integer_
    }
    complete <- consensus$complete_consensus_k
    if (is.null(complete) && !is.na(n_clusters)) {
      complete <- n_clusters == candidate_k
    }
    complete <- if (is.logical(complete) && length(complete) == 1L &&
                      !is.na(complete)) {
      complete
    } else {
      NA
    }
    assignment_coverage <- consensus$assignment_coverage
    if (is.null(assignment_coverage) && !is.null(assignment_count)) {
      assignment_coverage <- mean(assignment_count > 0L)
    }
    if (is.null(assignment_coverage) && !is.null(labels)) {
      assignment_coverage <- mean(!is.na(labels))
    }
    consensus_label_coverage <- consensus$consensus_label_coverage
    if (is.null(consensus_label_coverage) && !is.null(labels)) {
      consensus_label_coverage <- mean(!is.na(labels))
    }
    if (is.null(consensus_label_coverage)) {
      consensus_label_coverage <- assignment_coverage
    }
    replicated_coverage <- consensus$replicated_assignment_coverage
    if (is.null(replicated_coverage) && !is.null(assignment_count)) {
      replicated_coverage <- mean(assignment_count >= 2L)
    }
    scalar_coverage <- function(value) {
      if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
        as.numeric(value)
      } else {
        NA_real_
      }
    }

    data.frame(
      k = as.integer(candidate_k), status = "succeeded",
      computation_status = "computed",
      n_consensus_clusters = n_clusters,
      complete_consensus_k = complete,
      assignment_coverage = scalar_coverage(assignment_coverage),
      consensus_label_coverage = scalar_coverage(consensus_label_coverage),
      replicated_assignment_coverage = scalar_coverage(replicated_coverage),
      stringsAsFactors = FALSE
    )
  }))
}

.workflow_cross_model_summary <- function(object) {
  cross_models <- object$cross_models
  if (is.null(cross_models)) {
    return(data.frame(
      method = character(), succeeded = integer(), failed = integer(),
      warnings = integer(), expected = integer(), success_rate = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  records <- cross_models$records %||% list()
  failures <- cross_models$failures %||% data.frame()
  warnings <- cross_models$warnings %||% data.frame()
  record_methods <- if (length(records)) {
    vapply(records, function(record) {
      method <- record$method
      if (is.null(method) || !length(method) || is.na(method[[1L]])) {
        NA_character_
      } else {
        as.character(method[[1L]])
      }
    }, character(1))
  } else {
    character()
  }
  table_methods <- function(table) {
    if (is.data.frame(table) && "method" %in% names(table)) {
      as.character(table$method)
    } else {
      character()
    }
  }
  methods <- unique(c(
    as.character(cross_models$methods %||% character()),
    record_methods,
    table_methods(failures),
    table_methods(warnings)
  ))
  methods <- methods[!is.na(methods) & nzchar(methods)]
  if (!length(methods)) {
    return(data.frame(
      method = character(), succeeded = integer(), failed = integer(),
      warnings = integer(), expected = integer(), success_rate = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(methods, function(method) {
    succeeded <- sum(record_methods == method, na.rm = TRUE)
    failed <- sum(table_methods(failures) == method, na.rm = TRUE)
    warning_count <- sum(table_methods(warnings) == method, na.rm = TRUE)
    expected <- succeeded + failed
    data.frame(
      method = method,
      succeeded = as.integer(succeeded),
      failed = as.integer(failed),
      warnings = as.integer(warning_count),
      expected = as.integer(expected),
      success_rate = if (expected > 0L) succeeded / expected else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

.format_workflow_rate <- function(value) {
  if (length(value) == 1L && is.finite(value)) {
    sprintf("%.1f%%", 100 * value)
  } else {
    "NA"
  }
}

.cross_model_print_fields <- function(cross_models) {
  expected <- if ("expected" %in% names(cross_models)) {
    cross_models$expected
  } else {
    cross_models$succeeded + cross_models$failed
  }
  success_rate <- if ("success_rate" %in% names(cross_models)) {
    cross_models$success_rate
  } else {
    ifelse(
      !is.na(expected) & expected > 0,
      cross_models$succeeded / expected,
      NA_real_
    )
  }
  list(expected = expected, success_rate = success_rate)
}

.consensus_computation_status <- function(consensus) {
  if ("computation_status" %in% names(consensus)) {
    return(as.character(consensus$computation_status))
  }
  if ("status" %in% names(consensus)) {
    return(ifelse(consensus$status == "succeeded", "computed", "not_computed"))
  }
  rep("not_computed", nrow(consensus))
}

.print_workflow_consensus <- function(consensus) {
  computation_status <- .consensus_computation_status(consensus)
  detail_columns <- c(
    "complete_consensus_k", "assignment_coverage",
    "consensus_label_coverage", "replicated_assignment_coverage"
  )
  has_details <- all(detail_columns %in% names(consensus))
  for (i in seq_len(nrow(consensus))) {
    if (computation_status[[i]] == "not_computed") {
      cat(sprintf("    - k=%d: not_computed\n", consensus$k[[i]]))
    } else if (!has_details) {
      cat(sprintf("    - k=%d: computed\n", consensus$k[[i]]))
    } else {
      complete <- if (is.na(consensus$complete_consensus_k[[i]])) {
        "NA"
      } else if (consensus$complete_consensus_k[[i]]) {
        "yes"
      } else {
        "no"
      }
      cat(sprintf(
        paste0(
          "    - k=%d: computed; complete=%s; assignment=%s; ",
          "labels=%s; replicated=%s\n"
        ),
        consensus$k[[i]], complete,
        .format_workflow_rate(consensus$assignment_coverage[[i]]),
        .format_workflow_rate(consensus$consensus_label_coverage[[i]]),
        .format_workflow_rate(consensus$replicated_assignment_coverage[[i]])
      ))
    }
  }
}

#' @export
print.som_workflow <- function(x, ...) {
  workflow_summary <- summary(x)
  som_success <- sum(vapply(
    x$ensemble$fits, function(fit) isTRUE(fit$success), logical(1)
  ))
  cat("<som_workflow>\n")
  cat("  SOM fits      :", x$ensemble$expected_models, "attempted;",
      som_success, "succeeded;", nrow(x$ensemble$failures), "failed;",
      .count_noun(nrow(x$ensemble$warnings), "warning"), "\n")
  cat("  candidate k   :", paste(
    x$requested_k %||% x$partitions$stability$k, collapse = ", "
  ), "\n")
  consensus_status <- workflow_summary$consensus
  consensus_computation <- .consensus_computation_status(
    consensus_status
  )
  cat("  consensus     :", sum(
    consensus_computation == "computed"
  ), "computed;", sum(
    consensus_computation == "not_computed"
  ),
  "not_computed\n")
  .print_workflow_consensus(consensus_status)
  method_status <- workflow_summary$cross_models
  if (nrow(method_status)) {
    total_expected <- sum(method_status$expected)
    total_succeeded <- sum(method_status$succeeded)
    total_failed <- sum(method_status$failed)
    total_warnings <- sum(method_status$warnings)
    total_rate <- if (total_expected > 0L) {
      total_succeeded / total_expected
    } else {
      NA_real_
    }
    cat("  cross-model   :", total_expected, "expected;", total_succeeded,
        "succeeded;", total_failed, "failed;",
        .count_noun(total_warnings, "warning"), ";",
        .format_workflow_rate(total_rate), "success\n")
    for (i in seq_len(nrow(method_status))) {
      cat(sprintf(
        paste0(
          "    - %s: %d expected, %d succeeded, %d failed, %s, ",
          "%s success\n"
        ),
        method_status$method[[i]], method_status$expected[[i]],
        method_status$succeeded[[i]], method_status$failed[[i]],
        .count_noun(method_status$warnings[[i]], "warning"),
        .format_workflow_rate(method_status$success_rate[[i]])
      ))
    }
  }
  invisible(x)
}

#' Summarize workflow quality assurance and model outcomes
#'
#' @param object A `som_workflow` object.
#' @param ... Reserved for future methods.
#'
#' @return A `summary.som_workflow` object containing fit, consensus,
#'   cross-model and provenance summaries plus the original failure tables.
#' @export
summary.som_workflow <- function(object, ...) {
  som_success <- sum(vapply(
    object$ensemble$fits, function(fit) isTRUE(fit$success), logical(1)
  ))
  som <- data.frame(
    attempted = object$ensemble$expected_models,
    succeeded = som_success,
    failed = nrow(object$ensemble$failures),
    warnings = nrow(object$ensemble$warnings),
    success_rate = object$audit$success_rate,
    stringsAsFactors = FALSE
  )
  requested_k <- object$requested_k %||% .workflow_requested_k(object)
  consensus <- .workflow_consensus_summary(object, requested_k)
  cross_models <- .workflow_cross_model_summary(object)
  structure(
    list(
      som = som,
      consensus = consensus,
      cross_models = cross_models,
      failures = list(
        som = object$ensemble$failures,
        consensus = object$consensus_failures,
        cross_models = if (is.null(object$cross_models)) {
          data.frame()
        } else {
          object$cross_models$failures
        }
      ),
      warnings = list(
        som = object$ensemble$warnings,
        cross_models = if (is.null(object$cross_models)) {
          data.frame()
        } else {
          object$cross_models$warnings
        }
      ),
      provenance = object$provenance
    ),
    class = "summary.som_workflow"
  )
}

#' @export
print.summary.som_workflow <- function(x, ...) {
  cat("<summary.som_workflow>\n")
  cat("  SOM success:", x$som$succeeded, "/", x$som$attempted, "\n")
  consensus_computation <- .consensus_computation_status(x$consensus)
  cat("  consensus  :", sum(
    consensus_computation == "computed"
  ), "computed;", sum(
    consensus_computation == "not_computed"
  ), "not_computed\n")
  .print_workflow_consensus(x$consensus)
  if (nrow(x$cross_models)) {
    print_fields <- .cross_model_print_fields(x$cross_models)
    cat("  reference methods:\n")
    for (i in seq_len(nrow(x$cross_models))) {
      cat(sprintf(
        paste0(
          "    - %s: %d expected, %d succeeded, %d failed, %s, ",
          "%s success\n"
        ),
        x$cross_models$method[[i]], print_fields$expected[[i]],
        x$cross_models$succeeded[[i]], x$cross_models$failed[[i]],
        .count_noun(x$cross_models$warnings[[i]], "warning"),
        .format_workflow_rate(print_fields$success_rate[[i]])
      ))
    }
  }
  invisible(x)
}

.summarise_workflow <- function(workflow, scenario) {
  representation <- workflow$audit$grid_summary
  representation$scenario <- rep(scenario, nrow(representation))
  representation <- representation[, c(
    "scenario", setdiff(names(representation), "scenario")
  )]

  partition <- workflow$partitions$stability
  partition$scenario <- rep(scenario, nrow(partition))
  partition$consensus_method <- rep(NA_character_, nrow(partition))
  partition$consensus_assignment_coverage <- rep(NA_real_, nrow(partition))
  partition$consensus_label_coverage <- rep(NA_real_, nrow(partition))
  partition$replicated_assignment_coverage <- rep(NA_real_, nrow(partition))
  partition$min_cluster_jaccard <- rep(NA_real_, nrow(partition))
  partition$median_membership_support <- rep(NA_real_, nrow(partition))
  partition$median_assignment_entropy <- rep(NA_real_, nrow(partition))
  for (i in seq_len(nrow(partition))) {
    consensus <- workflow$consensus[[paste0("k", partition$k[[i]])]]
    if (!is.null(consensus)) {
      partition$consensus_method[[i]] <- consensus$method
      partition$consensus_assignment_coverage[[i]] <-
        consensus$assignment_coverage
      partition$consensus_label_coverage[[i]] <-
        consensus$consensus_label_coverage %||% consensus$assignment_coverage
      partition$replicated_assignment_coverage[[i]] <-
        consensus$replicated_assignment_coverage
      cluster_values <- consensus$cluster_summary$median_jaccard
      partition$min_cluster_jaccard[[i]] <- if (
        length(cluster_values) && all(is.finite(cluster_values))
      ) min(cluster_values) else NA_real_
      partition$median_membership_support[[i]] <- stats::median(
        consensus$membership_support,
        na.rm = TRUE
      )
      partition$median_assignment_entropy[[i]] <- stats::median(
        consensus$assignment_entropy,
        na.rm = TRUE
      )
    }
  }
  partition <- partition[, c("scenario", setdiff(names(partition), "scenario"))]

  cross_model <- if (is.null(workflow$cross_comparison)) {
    data.frame()
  } else {
    out <- workflow$cross_comparison$summary
    out$scenario <- rep(scenario, nrow(out))
    out[, c("scenario", setdiff(names(out), "scenario"))]
  }
  list(
    representation = representation,
    partition = partition,
    cross_model = cross_model
  )
}

.empty_sensitivity_comparison <- function() {
  data.frame(
    scenario_a = character(), scenario_b = character(), k = integer(),
    n_shared = integer(), n_replicated_a = integer(),
    n_replicated_b = integer(), n_joint = integer(),
    joint_coverage = numeric(), ari = numeric(), ami = numeric(),
    comparison_status = character(), stringsAsFactors = FALSE
  )
}

.empty_sample_comparison <- function() {
  data.frame(
    scenario_a = character(), scenario_b = character(), k = integer(),
    sample_id = character(), n_joint = integer(),
    cluster_size_a = integer(), cluster_size_b = integer(),
    shared_cluster_size_a = integer(), shared_cluster_size_b = integer(),
    cluster_intersection = integer(), shared_cluster_union = integer(),
    all_cluster_union = integer(), membership_jaccard_shared = numeric(),
    membership_jaccard_all = numeric(), stringsAsFactors = FALSE
  )
}

.empty_sample_summary <- function() {
  data.frame(
    k = integer(), sample_id = character(), n_scenarios = integer(),
    n_contrasts = integer(), n_comparable_contrasts = integer(),
    n_possible_contrasts = integer(), contrast_coverage = numeric(),
    conditional_contrast_coverage = numeric(),
    median_membership_jaccard_shared = numeric(),
    membership_jaccard_shared_q025 = numeric(),
    membership_jaccard_shared_q975 = numeric(),
    min_membership_jaccard_shared = numeric(),
    median_membership_jaccard_all = numeric(),
    membership_jaccard_all_q025 = numeric(),
    membership_jaccard_all_q975 = numeric(),
    min_membership_jaccard_all = numeric(), median_joint_n = numeric(),
    stringsAsFactors = FALSE
  )
}

.workflow_requested_k <- function(workflow) {
  successful <- suppressWarnings(as.integer(sub("^k", "", names(workflow$consensus))))
  failed <- workflow$consensus_failures$k
  partitioned <- workflow$partitions$stability$k
  sort(unique(c(successful, failed, partitioned)))
}

.sensitivity_requested_k <- function(arguments) {
  candidate_k <- arguments[["k"]]
  if (is.null(candidate_k) && is.list(arguments[["spec"]])) {
    candidate_k <- arguments[["spec"]][["k"]]
  }
  if (!is.numeric(candidate_k) || !length(candidate_k) ||
        anyNA(candidate_k) || any(!is.finite(candidate_k)) ||
        any(candidate_k %% 1 != 0) || any(candidate_k < 2)) {
    return(integer())
  }
  sort(unique(as.integer(candidate_k)))
}

.sensitivity_id_source <- function(data) {
  source <- data$id_source
  if (is.character(source) && length(source) == 1L &&
        source %in% c("provided", "rownames", "generated")) {
    return(source)
  }
  "unknown"
}

.sensitivity_id_status <- function(first, second) {
  first_data <- first$ensemble$data
  second_data <- second$ensemble$data
  first_source <- .sensitivity_id_source(first_data)
  second_source <- .sensitivity_id_source(second_data)
  if ("unknown" %in% c(first_source, second_source)) {
    return("id_provenance_unavailable")
  }
  if ("generated" %in% c(first_source, second_source)) {
    return("unstable_generated_ids")
  }
  NULL
}

.sensitivity_unavailable_row <- function(pair, k, status) {
  data.frame(
    scenario_a = pair[[1L]],
    scenario_b = pair[[2L]],
    k = as.integer(k),
    n_shared = NA_integer_,
    n_replicated_a = NA_integer_,
    n_replicated_b = NA_integer_,
    n_joint = NA_integer_,
    joint_coverage = NA_real_,
    ari = NA_real_,
    ami = NA_real_,
    comparison_status = status,
    stringsAsFactors = FALSE
  )
}

.compare_sensitivity_consensus <- function(workflows, requested_k = NULL) {
  if (is.null(requested_k)) {
    requested_k <- lapply(workflows, .workflow_requested_k)
  }
  if (length(requested_k) < 2L) return(.empty_sensitivity_comparison())
  scenario_pairs <- utils::combn(names(requested_k), 2L, simplify = FALSE)
  rows <- list()
  cursor <- 0L
  for (pair in scenario_pairs) {
    first <- workflows[[pair[[1L]]]]
    second <- workflows[[pair[[2L]]]]
    scenario_failed <- is.null(first) || is.null(second)
    common_k <- intersect(
      requested_k[[pair[[1L]]]], requested_k[[pair[[2L]]]]
    )
    if (scenario_failed) {
      unavailable <- if (is.null(first) && is.null(second)) {
        "scenario_failed_both"
      } else if (is.null(first)) {
        "scenario_failed_a"
      } else {
        "scenario_failed_b"
      }
      if (!length(common_k)) {
        cursor <- cursor + 1L
        rows[[cursor]] <- .sensitivity_unavailable_row(
          pair, NA_integer_, unavailable
        )
        next
      }
      for (candidate_k in common_k) {
        cursor <- cursor + 1L
        rows[[cursor]] <- .sensitivity_unavailable_row(
          pair, candidate_k, unavailable
        )
      }
      next
    }
    if (!length(common_k)) {
      cursor <- cursor + 1L
      rows[[cursor]] <- .sensitivity_unavailable_row(
        pair, NA_integer_, "no_common_k"
      )
      next
    }
    id_status <- .sensitivity_id_status(first, second)
    for (candidate_k in common_k) {
      if (!is.null(id_status)) {
        cursor <- cursor + 1L
        rows[[cursor]] <- .sensitivity_unavailable_row(
          pair, candidate_k, id_status
        )
        next
      }
      key <- paste0("k", candidate_k)
      first_consensus <- first$consensus[[key]]
      second_consensus <- second$consensus[[key]]
      if (is.null(first_consensus) || is.null(second_consensus)) {
        unavailable <- if (is.null(first_consensus) && is.null(second_consensus)) {
          "consensus_unavailable_both"
        } else if (is.null(first_consensus)) {
          "consensus_unavailable_a"
        } else {
          "consensus_unavailable_b"
        }
        cursor <- cursor + 1L
        rows[[cursor]] <- .sensitivity_unavailable_row(
          pair, candidate_k, unavailable
        )
        next
      }
      first_ids <- first_consensus$metadata$id
      second_ids <- second_consensus$metadata$id
      shared_ids <- intersect(first_ids, second_ids)
      first_index <- match(shared_ids, first_ids)
      second_index <- match(shared_ids, second_ids)
      first_replicated <- first_consensus$assignment_count[first_index] >= 2L
      second_replicated <-
        second_consensus$assignment_count[second_index] >= 2L
      jointly_replicated <- first_replicated & second_replicated
      first_labels <- first_consensus$consensus_labels[first_index]
      second_labels <- second_consensus$consensus_labels[second_index]
      jointly_evaluable <- jointly_replicated & !is.na(first_labels) &
        !is.na(second_labels)
      n_shared <- length(shared_ids)
      n_joint <- sum(jointly_evaluable)
      agreement <- .partition_agreement(
        first_labels[jointly_evaluable],
        second_labels[jointly_evaluable]
      )
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        scenario_a = pair[[1L]],
        scenario_b = pair[[2L]],
        k = first_consensus$k,
        n_shared = n_shared,
        n_replicated_a = sum(first_replicated),
        n_replicated_b = sum(second_replicated),
        n_joint = n_joint,
        joint_coverage = if (n_shared) n_joint / n_shared else NA_real_,
        ari = agreement[["ari"]],
        ami = agreement[["ami"]],
        comparison_status = if (n_joint >= 2L) {
          "evaluated"
        } else if (n_shared) {
          "insufficient_joint_assignments"
        } else {
          "no_shared_samples"
        },
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else .empty_sensitivity_comparison()
}

.sample_membership_jaccard <- function(
  pair, candidate_k, first_consensus, second_consensus
) {
  first_ids <- first_consensus$metadata$id
  second_ids <- second_consensus$metadata$id
  first_all_labels <- first_consensus$consensus_labels
  second_all_labels <- second_consensus$consensus_labels
  first_eligible <- first_consensus$assignment_count >= 2L &
    !is.na(first_all_labels)
  second_eligible <- second_consensus$assignment_count >= 2L &
    !is.na(second_all_labels)
  shared_ids <- intersect(first_ids, second_ids)
  first_index <- match(shared_ids, first_ids)
  second_index <- match(shared_ids, second_ids)
  first_labels <- first_all_labels[first_index]
  second_labels <- second_all_labels[second_index]
  jointly_evaluable <-
    first_eligible[first_index] & second_eligible[second_index]
  if (sum(jointly_evaluable) < 2L) {
    return(.empty_sample_comparison())
  }

  sample_id <- shared_ids[jointly_evaluable]
  labels_a <- factor(first_labels[jointly_evaluable])
  labels_b <- factor(second_labels[jointly_evaluable])
  contingency <- table(labels_a, labels_b)
  shared_cluster_size_a <- as.integer(
    table(labels_a)[as.character(labels_a)]
  )
  shared_cluster_size_b <- as.integer(
    table(labels_b)[as.character(labels_b)]
  )
  cluster_size_a <- as.integer(
    table(first_all_labels[first_eligible])[as.character(labels_a)]
  )
  cluster_size_b <- as.integer(
    table(second_all_labels[second_eligible])[as.character(labels_b)]
  )
  intersection <- as.integer(
    contingency[cbind(as.integer(labels_a), as.integer(labels_b))]
  )
  shared_union <- shared_cluster_size_a + shared_cluster_size_b - intersection
  all_union <- cluster_size_a + cluster_size_b - intersection

  data.frame(
    scenario_a = pair[[1L]],
    scenario_b = pair[[2L]],
    k = as.integer(candidate_k),
    sample_id = sample_id,
    n_joint = rep.int(length(sample_id), length(sample_id)),
    cluster_size_a = cluster_size_a,
    cluster_size_b = cluster_size_b,
    shared_cluster_size_a = shared_cluster_size_a,
    shared_cluster_size_b = shared_cluster_size_b,
    cluster_intersection = intersection,
    shared_cluster_union = shared_union,
    all_cluster_union = all_union,
    membership_jaccard_shared = intersection / shared_union,
    membership_jaccard_all = intersection / all_union,
    stringsAsFactors = FALSE
  )
}

.compare_sensitivity_samples <- function(workflows, scenario_comparison) {
  evaluated <- scenario_comparison[
    scenario_comparison$comparison_status == "evaluated", , drop = FALSE
  ]
  if (!nrow(evaluated)) {
    return(list(
      comparison = .empty_sample_comparison(),
      summary = .empty_sample_summary()
    ))
  }

  comparisons <- lapply(seq_len(nrow(evaluated)), function(i) {
    pair <- c(evaluated$scenario_a[[i]], evaluated$scenario_b[[i]])
    candidate_k <- evaluated$k[[i]]
    key <- paste0("k", candidate_k)
    .sample_membership_jaccard(
      pair = pair,
      candidate_k = candidate_k,
      first_consensus = workflows[[pair[[1L]]]]$consensus[[key]],
      second_consensus = workflows[[pair[[2L]]]]$consensus[[key]]
    )
  })
  comparisons <- Filter(function(x) nrow(x) > 0L, comparisons)
  if (!length(comparisons)) {
    return(list(
      comparison = .empty_sample_comparison(),
      summary = .empty_sample_summary()
    ))
  }
  comparison <- do.call(rbind, comparisons)

  group_key <- interaction(
    comparison$k, comparison$sample_id, drop = TRUE, lex.order = TRUE
  )
  grouped <- split(comparison, group_key)
  comparable_by_k <- table(evaluated$k)
  planned <- scenario_comparison[!is.na(scenario_comparison$k), , drop = FALSE]
  possible_by_k <- table(planned$k)
  summary <- do.call(rbind, lapply(grouped, function(x) {
    shared_interval <- .quantile_safe(
      x$membership_jaccard_shared, c(0.025, 0.975)
    )
    all_interval <- .quantile_safe(
      x$membership_jaccard_all, c(0.025, 0.975)
    )
    scenarios <- unique(c(x$scenario_a, x$scenario_b))
    n_comparable <- as.integer(
      comparable_by_k[as.character(x$k[[1L]])]
    )
    n_possible <- as.integer(possible_by_k[as.character(x$k[[1L]])])
    data.frame(
      k = x$k[[1L]],
      sample_id = x$sample_id[[1L]],
      n_scenarios = length(scenarios),
      n_contrasts = nrow(x),
      n_comparable_contrasts = n_comparable,
      n_possible_contrasts = n_possible,
      contrast_coverage = nrow(x) / n_possible,
      conditional_contrast_coverage = nrow(x) / n_comparable,
      median_membership_jaccard_shared = stats::median(
        x$membership_jaccard_shared
      ),
      membership_jaccard_shared_q025 = shared_interval[[1L]],
      membership_jaccard_shared_q975 = shared_interval[[2L]],
      min_membership_jaccard_shared = min(x$membership_jaccard_shared),
      median_membership_jaccard_all = stats::median(
        x$membership_jaccard_all
      ),
      membership_jaccard_all_q025 = all_interval[[1L]],
      membership_jaccard_all_q975 = all_interval[[2L]],
      min_membership_jaccard_all = min(x$membership_jaccard_all),
      median_joint_n = stats::median(x$n_joint),
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary) <- NULL
  list(comparison = comparison, summary = summary)
}

#' Run prespecified SOM sensitivity scenarios
#'
#' Each named scenario must provide `data` and `spec`, and may provide
#' `resamples`, `preprocess` and `k`. Scenarios may represent alternative
#' variable modules, transformations, layer weights, grids or data-coverage
#' rules. Results remain separate by scenario and evidence stream; the function
#' creates no aggregate ranking.
#'
#' @section Lifecycle:
#' Experimental. The scenario-list interface may change after independent
#' usability testing. Individual workflow functions and their evidence tables
#' are the candidate-stable interfaces.
#'
#' @param scenarios A named list of workflow argument lists.
#' @param cross_models Reference methods used in every scenario.
#' @param cross_model_control Named reference-model controls passed to every
#'   scenario through [run_som_workflow()].
#' @param consensus_method Consensus method used in every scenario.
#' @param max_coassignment_n Dense co-assignment limit.
#' @param max_pairwise_comparisons Pairwise partition-comparison budget passed
#'   to each scenario.
#' @param sample_profiles Whether to compute sample-level membership-set
#'   contrasts. Set to `FALSE` when the number of scenarios and samples would
#'   make the pairwise long table unnecessarily large.
#' @param keep_workflows Whether to retain full workflow objects.
#' @param fail_fast Whether to stop at the first scenario failure.
#'
#' @return A `som_sensitivity` object with tidy evidence tables, shared-sample
#'   comparisons between scenario consensus partitions, sample-level
#'   membership-set comparisons, scenario failures and optionally full
#'   workflows. Scenario agreement uses only samples assigned by at least two
#'   ensemble members in both scenarios, and at least two jointly evaluable
#'   samples are required before co-membership can be compared. For each focal
#'   sample, `membership_jaccard_shared` conditions on the jointly evaluable
#'   sample universe and isolates repartitioning, whereas
#'   `membership_jaccard_all` also includes eligible members unique to either
#'   scenario and therefore combines coverage and repartitioning effects.
#'   Neither requires aligning arbitrary cluster labels. `sample_summary`
#'   reports both distributions and distinguishes coverage of all planned
#'   contrasts from coverage conditional on globally comparable contrasts.
#'   These are sensitivity diagnostics, not confidence probabilities.
#'   Comparisons across
#'   different data objects require supplied identifiers or explicit row names;
#'   unsafe default identifiers, failed scenarios, unavailable consensus and
#'   absent common `k` values remain visible through `comparison_status` rather
#'   than being silently omitted. Legacy objects without identifier
#'   provenance must be reconstructed with an explicit `id` argument before
#'   cross-object comparison. Candidate `k` values are read exactly from each
#'   scenario or its specification; malformed values are not coerced. A failed
#'   scenario without a valid candidate set remains visible with `k = NA` but
#'   cannot contribute to a candidate-specific contrast denominator.
#' @export
run_som_sensitivity <- function(
  scenarios,
  cross_models = c("kmeans", "ward"),
  cross_model_control = list(),
  consensus_method = "auto",
  max_coassignment_n = 5000L,
  max_pairwise_comparisons = 1000000L,
  sample_profiles = TRUE,
  keep_workflows = TRUE,
  fail_fast = FALSE
) {
  if (!is.list(scenarios) || !length(scenarios) || is.null(names(scenarios)) ||
        any(names(scenarios) == "") || anyDuplicated(names(scenarios))) {
    .abort("`scenarios` must be a non-empty list with unique, non-empty names.")
  }
  .assert_flag(keep_workflows, "keep_workflows")
  .assert_flag(fail_fast, "fail_fast")
  .assert_flag(sample_profiles, "sample_profiles")

  workflows <- list()
  failures <- list()
  summaries <- list()
  requested_k <- list()
  model_failures <- list()
  model_warnings <- list()
  for (scenario in names(scenarios)) {
    arguments <- scenarios[[scenario]]
    if (!is.list(arguments) || is.null(arguments$data) || is.null(arguments$spec)) {
      .abort("Every sensitivity scenario must provide `data` and `spec`.")
    }
    requested_k[[scenario]] <- .sensitivity_requested_k(arguments)
    arguments$cross_models <- cross_models
    arguments$cross_model_control <- cross_model_control
    arguments$consensus_method <- consensus_method
    arguments$max_coassignment_n <- max_coassignment_n
    arguments$max_pairwise_comparisons <- max_pairwise_comparisons
    result <- tryCatch(
      do.call(run_som_workflow, arguments),
      error = function(e) {
        if (fail_fast) stop(e)
        e
      }
    )
    if (inherits(result, "error")) {
      failures[[length(failures) + 1L]] <- data.frame(
        scenario = scenario,
        error = conditionMessage(result),
        stringsAsFactors = FALSE
      )
    } else {
      workflows[[scenario]] <- result
      summaries[[scenario]] <- .summarise_workflow(result, scenario)
      if (nrow(result$ensemble$failures)) {
        model_failures[[length(model_failures) + 1L]] <- data.frame(
          scenario = scenario,
          stage = "som",
          id = result$ensemble$failures$id,
          method = "som",
          k = NA_integer_,
          message = result$ensemble$failures$error,
          stringsAsFactors = FALSE
        )
      }
      if (nrow(result$consensus_failures)) {
        model_failures[[length(model_failures) + 1L]] <- data.frame(
          scenario = scenario,
          stage = "consensus",
          id = paste0("k", result$consensus_failures$k),
          method = "consensus",
          k = result$consensus_failures$k,
          message = result$consensus_failures$error,
          stringsAsFactors = FALSE
        )
      }
      if (!is.null(result$cross_models) &&
            nrow(result$cross_models$failures)) {
        model_failures[[length(model_failures) + 1L]] <- data.frame(
          scenario = scenario,
          stage = "cross_model",
          id = result$cross_models$failures$id,
          method = result$cross_models$failures$method,
          k = result$cross_models$failures$k,
          message = result$cross_models$failures$error,
          stringsAsFactors = FALSE
        )
      }
      if (nrow(result$ensemble$warnings)) {
        model_warnings[[length(model_warnings) + 1L]] <- data.frame(
          scenario = scenario,
          stage = "som",
          id = result$ensemble$warnings$id,
          method = "som",
          k = NA_integer_,
          message = result$ensemble$warnings$warning,
          stringsAsFactors = FALSE
        )
      }
      if (!is.null(result$cross_models) &&
            nrow(result$cross_models$warnings)) {
        model_warnings[[length(model_warnings) + 1L]] <- data.frame(
          scenario = scenario,
          stage = "cross_model",
          id = result$cross_models$warnings$id,
          method = result$cross_models$warnings$method,
          k = result$cross_models$warnings$k,
          message = result$cross_models$warnings$warning,
          stringsAsFactors = FALSE
        )
      }
      if (identical(result$audit$success_rate, 0)) {
        messages <- unique(result$ensemble$failures$error)
        failures[[length(failures) + 1L]] <- data.frame(
          scenario = rep(scenario, length(messages)),
          error = paste0("All SOM fits failed: ", messages),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  failure_table <- if (length(failures)) {
    do.call(rbind, failures)
  } else {
    data.frame(scenario = character(), error = character())
  }
  bind_diagnostics <- function(x) {
    if (length(x)) {
      out <- do.call(rbind, x)
      rownames(out) <- NULL
      out
    } else {
      data.frame(
        scenario = character(), stage = character(), id = character(),
        method = character(), k = integer(), message = character(),
        stringsAsFactors = FALSE
      )
    }
  }
  bind_summary <- function(name) {
    tables <- lapply(summaries, `[[`, name)
    tables <- Filter(function(x) nrow(x) > 0L, tables)
    if (length(tables)) do.call(rbind, tables) else data.frame()
  }

  scenario_comparison <- .compare_sensitivity_consensus(
    workflows, requested_k
  )
  sample_evidence <- if (sample_profiles) {
    .compare_sensitivity_samples(workflows, scenario_comparison)
  } else {
    list(
      comparison = .empty_sample_comparison(),
      summary = .empty_sample_summary()
    )
  }

  structure(
    list(
      representation = bind_summary("representation"),
      partition = bind_summary("partition"),
      cross_model = bind_summary("cross_model"),
      scenario_comparison = scenario_comparison,
      sample_comparison = sample_evidence$comparison,
      sample_summary = sample_evidence$summary,
      failures = failure_table,
      model_failures = bind_diagnostics(model_failures),
      model_warnings = bind_diagnostics(model_warnings),
      workflows = if (keep_workflows) workflows else NULL
    ),
    class = "som_sensitivity"
  )
}

#' @export
print.som_sensitivity <- function(x, ...) {
  cat("<som_sensitivity>\n")
  cat("  successful scenarios:", length(unique(x$partition$scenario)), "\n")
  pair_key <- paste(
    x$scenario_comparison$scenario_a,
    x$scenario_comparison$scenario_b,
    sep = "\r"
  )
  evaluable_pairs <- unique(
    pair_key[x$scenario_comparison$comparison_status == "evaluated"]
  )
  unevaluable_pairs <- setdiff(unique(pair_key), evaluable_pairs)
  cat("  evaluable contrasts:", length(evaluable_pairs), "\n")
  cat("  unevaluable pairs   :", length(unevaluable_pairs), "\n")
  sample_profiles <- if (is.null(x$sample_summary)) {
    0L
  } else {
    nrow(x$sample_summary)
  }
  cat("  sample profiles     :", sample_profiles, "sample-k rows\n")
  cat("  failed scenarios    :", nrow(x$failures), "\n")
  cat("  model-stage failures:", nrow(x$model_failures %||% data.frame()), "\n")
  cat("  model-stage warnings:", nrow(x$model_warnings %||% data.frame()), "\n")
  cat("  aggregate ranking   : not computed\n")
  invisible(x)
}
