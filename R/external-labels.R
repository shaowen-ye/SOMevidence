.validate_external_ids <- function(ids, n, name) {
  if (length(ids) != n || anyNA(ids)) {
    .abort(sprintf("`%s` must contain one ID per supplied label.", name))
  }
  ids <- as.character(ids)
  if (any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
    .abort(sprintf("`%s` must contain unique, non-empty IDs.", name))
  }
  ids
}

.consensus_sample_ids <- function(consensus) {
  n <- length(consensus$consensus_labels)
  metadata <- consensus$metadata
  if (!is.data.frame(metadata) || !"id" %in% names(metadata)) {
    .abort("`consensus` has no valid sample-identity metadata; recompute it.")
  }
  metadata_ids <- .validate_external_ids(metadata$id, n, "consensus$metadata$id")
  sample_ids <- consensus$sample_ids %||% metadata_ids
  sample_ids <- .validate_external_ids(sample_ids, n, "consensus$sample_ids")
  if (!identical(sample_ids, metadata_ids)) {
    .abort(paste0(
      "`consensus$sample_ids` and `consensus$metadata$id` disagree; ",
      "recompute the consensus object."
    ))
  }
  ensemble_ids <- consensus$ensemble$data$metadata$id %||% NULL
  if (!is.null(ensemble_ids)) {
    ensemble_ids <- .validate_external_ids(
      ensemble_ids, n, "consensus$ensemble$data$metadata$id"
    )
    if (!identical(sample_ids, ensemble_ids)) {
      .abort(paste0(
        "Consensus sample IDs do not match the originating ensemble; ",
        "recompute the consensus object."
      ))
    }
  }
  resample_ids <- consensus$ensemble$resamples$sample_ids %||% NULL
  if (!is.null(resample_ids)) {
    resample_ids <- .validate_external_ids(
      resample_ids, n, "consensus$ensemble$resamples$sample_ids"
    )
    if (!identical(sample_ids, resample_ids)) {
      .abort(paste0(
        "Consensus sample IDs do not match the originating resamples; ",
        "recompute the consensus object."
      ))
    }
  }
  if (!is.numeric(consensus$assignment_count) ||
        length(consensus$assignment_count) != n ||
        anyNA(consensus$assignment_count) ||
        any(!is.finite(consensus$assignment_count)) ||
        any(consensus$assignment_count < 0) ||
        any(consensus$assignment_count %% 1 != 0)) {
    .abort("`consensus$assignment_count` is invalid; recompute the consensus object.")
  }
  if (!is.matrix(consensus$aligned_labels) ||
        nrow(consensus$aligned_labels) != n) {
    .abort("`consensus$aligned_labels` is not sample-aligned; recompute it.")
  }
  observed_assignments <- rowSums(!is.na(consensus$aligned_labels))
  if (!identical(
    as.numeric(consensus$assignment_count),
    as.numeric(observed_assignments)
  )) {
    .abort(paste0(
      "`consensus$assignment_count` disagrees with the available aligned ",
      "partitions; recompute the consensus object."
    ))
  }
  if (!is.null(consensus$coassignment) &&
        !identical(dim(consensus$coassignment), c(n, n))) {
    .abort("`consensus$coassignment` is not sample-aligned; recompute it.")
  }
  sample_ids
}

.external_label_ids <- function(labels, label_ids) {
  label_names <- names(labels)
  if (!is.null(label_names)) {
    label_names <- .validate_external_ids(
      label_names, length(labels), "names(labels)"
    )
  }
  if (!is.null(label_ids)) {
    label_ids <- .validate_external_ids(
      label_ids, length(labels), "label_ids"
    )
  }
  if (!is.null(label_names) && !is.null(label_ids) &&
        !identical(label_names, label_ids)) {
    .abort("`label_ids` and `names(labels)` must identify the same rows in the same order.")
  }
  if (!is.null(label_ids) && !is.null(label_names)) {
    list(ids = label_ids, source = "label_ids+names(labels)")
  } else if (!is.null(label_ids)) {
    list(ids = label_ids, source = "label_ids")
  } else if (!is.null(label_names)) {
    list(ids = label_names, source = "names(labels)")
  } else {
    list(ids = NULL, source = "none")
  }
}

.external_omission_counts <- function(status) {
  omitted_levels <- c(
    "label_id_absent",
    "external_label_missing",
    "external_label_excluded",
    "consensus_label_missing",
    "insufficient_consensus_replication"
  )
  counts <- table(factor(status[status != "used"], levels = omitted_levels))
  data.frame(
    status = omitted_levels,
    n = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

#' Compare a consensus partition with external ecological labels
#'
#' External labels are evaluated only after SOM fitting and consensus
#' construction. ARI and AMI quantify agreement; neither is called accuracy,
#' and agreement does not make the external labels ecological ground truth.
#'
#' @param consensus A `som_consensus` object.
#' @param labels Optional external labels. By default, the `external_label`
#'   field supplied to [som_data()] is used. Supplied labels should be matched
#'   by stable sample IDs through `label_ids` or complete `names(labels)`.
#' @param exclude Optional label values to omit, such as an explicit unknown
#'   category. Missing labels and samples without at least two analysis-scope
#'   assignments are always omitted and counted because one assignment cannot
#'   provide consensus evidence.
#' @param label_ids Optional unique sample identifiers corresponding to
#'   `labels`. They may be in any order and may describe a subset of consensus
#'   samples. IDs not present in the consensus are rejected.
#' @param match_by Matching rule. `"auto"` uses IDs when available. For an
#'   unnamed full-length label vector it warns and retains legacy positional
#'   matching. Use `"id"` to require identity matching or `"position"` to make
#'   full-length positional matching explicit.
#'
#' @return A `som_external_assessment` object containing the contingency table,
#'   long-form cluster composition, ARI, AMI, resolved matching information and
#'   mutually exclusive sample-omission accounting.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 9)
#' ensemble <- fit_som_ensemble(
#'   data, som_spec(c(3, 2), seeds = 10:11, rlen = 10, k = 2)
#' )
#' consensus <- consensus_som(partition_som(ensemble), k = 2)
#' labels <- stats::setNames(
#'   data$metadata$external_label,
#'   data$metadata$id
#' )
#' evaluate_external_labels(consensus, labels[sample(names(labels))])
#' @export
evaluate_external_labels <- function(
  consensus,
  labels = NULL,
  exclude = NULL,
  label_ids = NULL,
  match_by = c("auto", "id", "position")
) {
  if (!inherits(consensus, "som_consensus")) {
    .abort("`consensus` must come from `consensus_som()`.")
  }
  requested_match <- match.arg(match_by)
  consensus_ids <- .consensus_sample_ids(consensus)
  n_total <- length(consensus_ids)
  source <- "supplied"

  if (is.null(labels)) {
    if (!is.null(label_ids)) {
      .abort("`label_ids` cannot be supplied when `labels` is NULL.")
    }
    if (requested_match != "auto") {
      .abort("`match_by` applies only when `labels` are supplied explicitly.")
    }
    if (!"external_label" %in% names(consensus$metadata)) {
      .abort("No external labels are stored; supply `labels` explicitly.")
    }
    labels <- consensus$metadata$external_label
    if (length(labels) != n_total) {
      .abort("Stored external labels are not aligned with consensus samples.")
    }
    aligned_labels <- labels
    input_present <- rep(TRUE, n_total)
    input_ids <- consensus_ids
    external_label_source <-
      consensus$ensemble$data$external_label_source %||% "legacy"
    identifier_source <- switch(
      external_label_source,
      named_id = "som_data named-ID alignment",
      position = "som_data row binding",
      none = "som_data row binding",
      "legacy stored metadata"
    )
    match_method <- "stored"
    input_reordered <- FALSE
    source <- "som_data.external_label"
  } else {
    if (!is.atomic(labels) || !is.null(dim(labels))) {
      .abort("`labels` must be an atomic vector.")
    }
    identity <- .external_label_ids(labels, label_ids)
    input_ids <- identity$ids
    identifier_source <- identity$source

    match_method <- requested_match
    if (requested_match == "auto") {
      if (!is.null(input_ids)) {
        match_method <- "id"
      } else if (length(labels) == n_total) {
        warning(paste0(
          "Supplied labels have no sample IDs. Using legacy positional ",
          "matching; provide `label_ids`, name `labels`, or set ",
          "`match_by = \"position\"` explicitly."
        ), call. = FALSE)
        match_method <- "position"
      } else {
        .abort(paste0(
          "A label subset cannot be aligned without sample IDs; provide ",
          "`label_ids` or complete `names(labels)`."
        ))
      }
    }

    if (match_method == "id") {
      if (is.null(input_ids)) {
        .abort(paste0(
          "`match_by = \"id\"` requires `label_ids` or complete ",
          "`names(labels)`."
        ))
      }
      extra_ids <- setdiff(input_ids, consensus_ids)
      if (length(extra_ids)) {
        .abort(paste0(
          "External label IDs are absent from the consensus: ",
          paste(utils::head(extra_ids, 5L), collapse = ", "),
          if (length(extra_ids) > 5L) ", ..." else "",
          "."
        ))
      }
      aligned_index <- match(consensus_ids, input_ids)
      input_present <- !is.na(aligned_index)
      aligned_labels <- labels[aligned_index]
      input_reordered <- !identical(
        input_ids,
        consensus_ids[consensus_ids %in% input_ids]
      )
    } else {
      if (length(labels) != n_total) {
        .abort("Positional matching requires one label per consensus sample.")
      }
      if (!is.null(input_ids) && !identical(input_ids, consensus_ids)) {
        .abort(paste0(
          "Supplied label IDs do not match consensus row order; use ",
          "`match_by = \"id\"`."
        ))
      }
      aligned_labels <- labels
      input_present <- rep(TRUE, n_total)
      input_reordered <- FALSE
    }
  }

  status <- rep("used", n_total)
  status[!input_present] <- "label_id_absent"
  unresolved <- status == "used"
  status[unresolved & is.na(aligned_labels)] <- "external_label_missing"
  unresolved <- status == "used"
  if (!is.null(exclude)) {
    status[unresolved & aligned_labels %in% exclude] <-
      "external_label_excluded"
  }
  unresolved <- status == "used"
  status[unresolved & is.na(consensus$consensus_labels)] <-
    "consensus_label_missing"
  unresolved <- status == "used"
  status[unresolved & consensus$assignment_count < 2L] <-
    "insufficient_consensus_replication"

  keep <- status == "used"
  omission_counts <- .external_omission_counts(status)
  n_used <- sum(keep)
  n_omitted <- n_total - n_used
  if (n_used < 2L || length(unique(aligned_labels[keep])) < 2L) {
    omitted_text <- paste0(
      omission_counts$status, "=", omission_counts$n, collapse = ", "
    )
    .abort(paste0(
      "External assessment requires at least two represented label levels ",
      "after matching and exclusions (n_used=", n_used,
      "; omissions: ", omitted_text, ")."
    ))
  }

  partition <- consensus$consensus_labels[keep]
  external <- aligned_labels[keep]
  contingency <- table(
    consensus_cluster = partition,
    external_label = external,
    useNA = "no"
  )
  composition <- as.data.frame(contingency, stringsAsFactors = FALSE)
  names(composition)[[3L]] <- "n"
  cluster_n <- stats::ave(
    composition$n,
    composition$consensus_cluster,
    FUN = sum
  )
  composition$within_cluster_proportion <- composition$n / cluster_n
  agreement <- .partition_agreement(partition, external)
  sample_accounting <- data.frame(
    sample_id = consensus_ids,
    status = status,
    used = keep,
    stringsAsFactors = FALSE
  )
  matching <- list(
    requested = requested_match,
    method = match_method,
    identifier_source = identifier_source,
    n_input = as.integer(length(labels)),
    n_matched = as.integer(sum(input_present)),
    n_consensus_without_input = as.integer(sum(!input_present)),
    n_unmatched_input = 0L,
    input_reordered = isTRUE(input_reordered)
  )

  .new_som_object(
    list(
      n_total = n_total,
      n_used = n_used,
      n_omitted = n_omitted,
      source = source,
      excluded_values = exclude,
      match_method = match_method,
      matching = matching,
      omission_counts = omission_counts,
      sample_accounting = sample_accounting,
      ari = agreement[["ari"]],
      ami = agreement[["ami"]],
      contingency = contingency,
      composition = composition
    ),
    "som_external_assessment"
  )
}

#' @export
print.som_external_assessment <- function(x, ...) {
  cat("<som_external_assessment> (post hoc)\n")
  cat("  samples used:", x$n_used, "of", x$n_total, "\n")
  cat("  label match :", x$match_method %||% "legacy position", "\n")
  cat("  ARI         :", sprintf("%.3f", x$ari), "\n")
  cat("  AMI         :", sprintf("%.3f", x$ami), "\n")
  cat("  interpretation: agreement, not classification accuracy\n")
  invisible(x)
}
