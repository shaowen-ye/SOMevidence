#' Identify Pareto-efficient SOM candidates
#'
#' When `x` is a `som_audit`, fit-level metrics are directly comparable only
#' when every successful fit uses the same analysis rows. To preserve the
#' established interface, an audit with distinct analysis sets still returns
#' its exploratory fit-level frontier, but emits a warning and marks the result
#' with `attr(result, "comparison_scope") == "distinct_analysis_sets"`. Such a
#' frontier must not be used to choose a configuration. Instead, construct
#' prespecified configuration-level summaries over identical successful split
#' and seed coverage, then pass that justified comparable data frame.
#'
#' @param x A `som_audit` object or a data frame of comparable candidates.
#' @param metrics Named character vector whose values are `"min"` or `"max"`.
#'
#' @return The non-dominated rows, with a `pareto` column. Results derived from
#'   a `som_audit` also carry a `comparison_scope` attribute.
#' @export
pareto_candidates <- function(
  x,
  metrics = c(
    quantization_error = "min",
    topographic_error = "min",
    empty_unit_rate = "min"
  )
) {
  is_audit <- inherits(x, "som_audit")
  distinct_analysis_sets <- is_audit && .audit_has_distinct_sets(x)
  if (distinct_analysis_sets) {
    warning(paste0(
      "This exploratory fit-level frontier compares metrics from distinct ",
      "analysis sets and must not be used to choose a configuration. Construct ",
      "prespecified configuration summaries over identical successful split ",
      "and seed coverage for an inferential comparison."
    ), call. = FALSE)
  }
  data <- if (is_audit) x$fit_metrics else x
  if (!is.data.frame(data)) .abort("`x` must be a `som_audit` or data frame.")
  if (is.null(names(metrics)) || any(!metrics %in% c("min", "max")) ||
        !all(names(metrics) %in% names(data))) {
    .abort("`metrics` must name data columns and use only `min` or `max`.")
  }
  values <- as.matrix(data[, names(metrics), drop = FALSE])
  if (!is.numeric(values) || anyNA(values) || any(!is.finite(values))) {
    .abort("Pareto metrics must be finite and non-missing.")
  }
  for (j in seq_along(metrics)) {
    if (metrics[[j]] == "max") values[, j] <- -values[, j]
  }

  dominated <- logical(nrow(values))
  for (i in seq_len(nrow(values))) {
    no_worse <- rowSums(sweep(values, 2L, values[i, ], "<=") - 0) == ncol(values)
    strictly_better <- rowSums(sweep(values, 2L, values[i, ], "<") - 0) > 0
    dominated[[i]] <- any(no_worse & strictly_better)
  }
  out <- data[!dominated, , drop = FALSE]
  out$pareto <- TRUE
  rownames(out) <- NULL
  if (is_audit) {
    attr(out, "comparison_scope") <- if (distinct_analysis_sets) {
      "distinct_analysis_sets"
    } else {
      "common_analysis_set"
    }
  }
  out
}

.audit_has_distinct_sets <- function(audit) {
  splits <- audit$ensemble$resamples$splits %||% list()
  split_ids <- unique(as.character(audit$fit_metrics$split_id %||% character()))
  if (length(splits) < 2L || length(split_ids) < 2L) {
    return(FALSE)
  }
  splits <- Filter(function(split) split$id %in% split_ids, splits)
  if (length(splits) < 2L) {
    return(FALSE)
  }
  analysis_keys <- vapply(splits, function(split) {
    paste(sort(unique(split$analysis)), collapse = ",")
  }, character(1))
  length(unique(analysis_keys)) > 1L
}
