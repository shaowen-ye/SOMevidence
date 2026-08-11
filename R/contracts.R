.som_contract_version <- "1.2.0"

.som_required_components <- list(
  som_data = c("layers", "metadata", "id_source", "external_label_source"),
  som_preprocess = c("transform", "center", "scale", "zero_replacement"),
  som_resamples = c("method", "splits", "seed", "n", "sample_ids"),
  som_spec = c(
    "grids", "seeds", "rlen", "alpha", "radius", "topology",
    "neighbourhood", "toroidal", "mode", "k", "max_na_fraction",
    "layer_weights", "normalize_layers", "cores"
  ),
  som_ensemble = c(
    "data", "spec", "resamples", "preprocess", "fits", "failures",
    "warnings", "expected_models", "parallel"
  ),
  som_audit = c(
    "fit_metrics", "grid_summary", "success_rate", "failures", "ensemble"
  ),
  som_partitions = c(
    "records", "pairwise", "stability", "method", "scope",
    "max_pairwise_comparisons", "ensemble"
  ),
  som_consensus = c(
    "k", "method", "scope", "coassignment", "tree", "consensus_labels",
    "aligned_labels", "membership_support", "assignment_entropy",
    "assignment_count", "assignment_coverage", "consensus_label_coverage",
    "observed_consensus_clusters", "n_consensus_clusters",
    "complete_consensus_k", "replicated_assignment_coverage",
    "alignment_diagnostics", "clusterwise_jaccard", "cluster_summary",
    "sample_ids", "metadata", "records", "ensemble"
  ),
  som_cross_models = c(
    "records", "failures", "warnings", "methods", "k", "ensemble"
  ),
  som_cross_comparison = c(
    "comparisons", "summary", "failures", "warnings", "methods",
    "reference_status", "partition_completeness", "scope", "ensemble"
  ),
  som_workflow = c(
    "ensemble", "audit", "partitions", "consensus", "consensus_failures",
    "cross_models", "cross_comparison", "requested_k", "provenance"
  ),
  som_gate = c(
    "max_topographic_error", "max_empty_unit_rate", "min_median_ari",
    "min_median_ami", "min_pairwise_coverage", "min_cluster_jaccard",
    "min_consensus_coverage", "min_replicated_coverage",
    "min_membership_support", "max_assignment_entropy",
    "min_cross_model_ari", "min_cross_model_methods", "min_success_rate"
  ),
  som_defensibility = c("status", "k", "evidence", "checks"),
  som_external_assessment = c(
    "n_total", "n_used", "n_omitted", "source", "excluded_values",
    "match_method", "matching", "omission_counts", "sample_accounting",
    "ari", "ami", "contingency", "composition"
  ),
  som_transfer_audit = c("metrics", "failures", "ensemble"),
  som_newdata_mapping = c(
    "records", "summary", "failures", "warnings", "metadata",
    "n_expected_fits"
  ),
  som_representation_audit = c(
    "fit_metrics", "pairwise", "neighbourhood_records", "summary",
    "failures", "warnings", "comparison_budget", "provenance", "ensemble"
  ),
  som_sensitivity = c(
    "representation", "partition", "cross_model", "scenario_comparison",
    "sample_comparison", "sample_summary", "failures", "model_failures",
    "model_warnings", "workflows"
  )
)

.new_som_object <- function(x, class) {
  structure(
    x,
    class = class,
    som_contract_version = .som_contract_version
  )
}

.som_public_class <- function(x) {
  recognised <- intersect(class(x), names(.som_required_components))
  if (length(recognised) != 1L) {
    .abort(paste0(
      "`x` must have exactly one supported public SOMevidence result class."
    ))
  }
  recognised[[1L]]
}

.validate_som_object <- function(x, class_name = .som_public_class(x)) {
  if (!is.list(x)) {
    .abort(sprintf("A `%s` object must be list-based.", class_name))
  }
  required <- .som_required_components[[class_name]]
  missing_components <- setdiff(required, names(x))
  if (length(missing_components)) {
    .abort(paste0(
      "The `", class_name, "` object lacks required components: ",
      paste(missing_components, collapse = ", "),
      ". Recompute it with the current package version."
    ))
  }
  invisible(x)
}

.validate_contract_version <- function(x) {
  version <- attr(x, "som_contract_version", exact = TRUE)
  if (is.null(version)) return(NULL)
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
        !nzchar(version)) {
    .abort("`som_contract_version` must be one non-empty version string.")
  }
  parsed <- tryCatch(numeric_version(version), error = function(e) NULL)
  target <- numeric_version(.som_contract_version)
  if (is.null(parsed)) {
    .abort("`som_contract_version` is not a valid version string.")
  }
  if (parsed > target) {
    .abort(paste0(
      "This object uses the newer contract version ", version,
      "; install a compatible SOMevidence release."
    ))
  }
  version
}

.upgrade_nested_som <- function(x, context_ids = NULL) {
  if (is.null(x)) return(NULL)
  .upgrade_som_object(x, context_ids = context_ids)
}

.validate_contract_ids <- function(ids, n, name) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
        !is.finite(n) || n %% 1 != 0 || n < 1L ||
        length(ids) != n || anyNA(ids)) {
    .abort(sprintf(
      "`%s` must contain one valid sample ID for each of %s rows.",
      name, format(n)
    ))
  }
  ids <- as.character(ids)
  if (any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
    .abort(sprintf("`%s` must contain unique, non-empty sample IDs.", name))
  }
  ids
}

.upgrade_som_object <- function(x, context_ids = NULL) {
  class_name <- .som_public_class(x)
  .validate_contract_version(x)

  if (class_name == "som_data") {
    if (is.null(x$external_label_source)) {
      x$external_label_source <- if (
        is.data.frame(x$metadata) && "external_label" %in% names(x$metadata)
      ) "legacy" else "none"
    }
  } else if (class_name == "som_resamples") {
    if (is.null(x$sample_ids)) {
      if (is.null(context_ids)) {
        .abort(paste0(
          "Legacy `som_resamples` objects without sample IDs cannot be ",
          "upgraded safely on their own. Recreate the resamples or upgrade ",
          "their originating ensemble."
        ))
      }
      x$sample_ids <- .validate_contract_ids(
        context_ids, x$n, "originating data IDs"
      )
    } else {
      x$sample_ids <- .validate_contract_ids(
        x$sample_ids, x$n, "som_resamples$sample_ids"
      )
      if (!is.null(context_ids)) {
        context_ids <- .validate_contract_ids(
          context_ids, x$n, "originating data IDs"
        )
        if (!identical(x$sample_ids, context_ids)) {
          .abort("Resampling sample IDs do not match the originating data.")
        }
      }
    }
  } else if (class_name == "som_ensemble") {
    x$data <- .upgrade_nested_som(x$data)
    data_ids <- x$data$metadata$id
    x$spec <- .upgrade_nested_som(x$spec)
    x$resamples <- .upgrade_nested_som(x$resamples, context_ids = data_ids)
  } else if (class_name %in% c(
    "som_audit", "som_partitions", "som_consensus", "som_cross_models",
    "som_cross_comparison", "som_transfer_audit",
    "som_representation_audit"
  )) {
    x$ensemble <- .upgrade_nested_som(x$ensemble)
    if (class_name == "som_partitions" &&
          is.null(x$max_pairwise_comparisons)) {
      x$max_pairwise_comparisons <- NA_integer_
    }
    if (class_name == "som_consensus") {
      if (is.null(x$sample_ids)) {
        x$sample_ids <- as.character(x$metadata$id)
      }
      invisible(.consensus_sample_ids(x))
    }
  } else if (class_name == "som_workflow") {
    cross_fields <- c("cross_models", "cross_comparison")
    if (!all(cross_fields %in% names(x))) {
      .abort(paste0(
        "A workflow must retain its cross-model fields, including explicit ",
        "NULL values; recompute the damaged workflow."
      ))
    }
    x$ensemble <- .upgrade_nested_som(x$ensemble)
    shared_ensemble <- x$ensemble
    upgrade_evidence <- function(object) {
      if (is.null(object)) return(NULL)
      object <- .upgrade_nested_som(object)
      if ("ensemble" %in% names(object)) {
        if (!identical(object$ensemble, shared_ensemble)) {
          .abort(paste0(
            "A nested workflow result has a different originating ensemble; ",
            "the workflow cannot be upgraded safely."
          ))
        }
        object$ensemble <- shared_ensemble
      }
      attr(object, "som_contract_version") <- .som_contract_version
      object
    }
    x$audit <- upgrade_evidence(x$audit)
    x$partitions <- upgrade_evidence(x$partitions)
    x$consensus <- lapply(x$consensus, upgrade_evidence)
    x[cross_fields] <- list(
      upgrade_evidence(x$cross_models),
      upgrade_evidence(x$cross_comparison)
    )
  } else if (class_name == "som_sensitivity" && !is.null(x$workflows)) {
    x$workflows <- lapply(x$workflows, function(workflow) {
      if (is.null(workflow)) NULL else .upgrade_nested_som(workflow)
    })
  }

  if (class_name == "som_external_assessment") {
    required <- .som_required_components[[class_name]]
    if (length(setdiff(required, names(x)))) {
      .abort(paste0(
        "Legacy `som_external_assessment` objects do not retain the ",
        "record-level matching and omission states required by contract ",
        .som_contract_version, ". Recompute the external assessment."
      ))
    }
  }

  .validate_som_object(x, class_name)
  attr(x, "som_contract_version") <- .som_contract_version
  x
}

#' Upgrade a persisted SOMevidence object contract
#'
#' Upgrades a supported public SOMevidence object to the current structural
#' contract using only deterministic, non-analytical migrations. The function
#' never refits a model or fabricates scientific evidence. If a required field
#' cannot be reconstructed, it asks the user to recompute the affected result.
#'
#' Contract versions describe object structure, not the package or algorithm
#' version that generated an analysis. Retain `som_workflow$provenance` and
#' `sessionInfo()` for that purpose.
#'
#' @param x A persisted public `som_*` object returned by SOMevidence.
#'
#' @return A validated object under the current contract. The input is not
#'   modified in place. Repeated upgrades are idempotent.
#' @examples
#' data <- som_data(matrix(seq_len(18), nrow = 6))
#' legacy <- data
#' attr(legacy, "som_contract_version") <- NULL
#' upgrade_som_object(legacy)
#' @export
upgrade_som_object <- function(x) {
  .upgrade_som_object(x)
}
