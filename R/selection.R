#' Identify Pareto-efficient SOM candidates
#'
#' @param x A `som_audit` object or data frame.
#' @param metrics Named character vector whose values are `"min"` or `"max"`.
#'
#' @return The non-dominated rows, with a `pareto` column.
#' @export
pareto_candidates <- function(
  x,
  metrics = c(
    quantization_error = "min",
    topographic_error = "min",
    empty_unit_rate = "min"
  )
) {
  data <- if (inherits(x, "som_audit")) x$fit_metrics else x
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
  out
}
