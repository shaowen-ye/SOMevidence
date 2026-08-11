#' Audit transfer to held-out sampling domains
#'
#' For each successful fit with assessment rows, the function contrasts mapping
#' error in the analysis and assessment sets and records the fraction of held-
#' out samples mapped to units unoccupied during training. These quantities
#' describe domain shift; they do not establish ecological transferability on
#' their own.
#'
#' @param ensemble A fitted `som_ensemble` containing non-empty assessment sets.
#'
#' @return A `som_transfer_audit` object with fit-level transfer diagnostics.
#' @examples
#' data <- simulate_som_scenario("gradient", n = 60, p = 3, seed = 7)
#' splits <- som_resamples(data, method = "leave_domain_out")
#' specification <- som_spec(c(3, 2), seeds = 1, rlen = 10, k = 2)
#' ensemble <- fit_som_ensemble(data, specification, splits)
#' audit_transfer(ensemble)
#' @export
audit_transfer <- function(ensemble) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`ensemble` must come from `fit_som_ensemble()`.")
  }
  successful <- Filter(function(x) {
    isTRUE(x$success) && length(x$assessment) > 0L
  }, ensemble$fits)
  if (!length(successful)) {
    .abort("Transfer auditing requires successful fits with assessment rows.")
  }
  metrics <- do.call(rbind, lapply(successful, function(fit) {
    analysis_distance <- fit$distances[fit$analysis]
    assessment_distance <- fit$distances[fit$assessment]
    analysis_bmu <- fit$bmu[fit$analysis]
    occupied <- unique(fit$bmu[fit$analysis])
    assessment_bmu <- fit$bmu[fit$assessment]
    analysis_mapped <- is.finite(analysis_distance) & !is.na(analysis_bmu)
    assessment_mapped <- is.finite(assessment_distance) & !is.na(assessment_bmu)
    train_median <- stats::median(analysis_distance, na.rm = TRUE)
    assessment_median <- stats::median(assessment_distance, na.rm = TRUE)
    data.frame(
      id = fit$id,
      split_id = fit$split_id,
      grid_id = fit$grid_id,
      seed = fit$seed,
      n_analysis = length(fit$analysis),
      n_assessment = length(fit$assessment),
      n_analysis_mapped = sum(analysis_mapped),
      n_assessment_mapped = sum(assessment_mapped),
      analysis_mapping_coverage = mean(analysis_mapped),
      assessment_mapping_coverage = mean(assessment_mapped),
      median_analysis_distance = train_median,
      median_assessment_distance = assessment_median,
      distance_ratio = if (train_median > 0) {
        assessment_median / train_median
      } else {
        NA_real_
      },
      unoccupied_unit_rate = .unoccupied_rate(assessment_bmu, occupied),
      stringsAsFactors = FALSE
    )
  }))
  .new_som_object(
    list(metrics = metrics, failures = ensemble$failures, ensemble = ensemble),
    "som_transfer_audit"
  )
}

#' @export
print.som_transfer_audit <- function(x, ...) {
  cat("<som_transfer_audit>\n")
  cat("  successful transfers:", nrow(x$metrics), "\n")
  cat("  median distance ratio:", sprintf(
    "%.3f", stats::median(x$metrics$distance_ratio, na.rm = TRUE)
  ), "\n")
  cat("  median held-out coverage:", sprintf(
    "%.3f", stats::median(
      x$metrics$assessment_mapping_coverage, na.rm = TRUE
    )
  ), "\n")
  cat("  median new-unit rate :", sprintf(
    "%.3f", stats::median(x$metrics$unoccupied_unit_rate, na.rm = TRUE)
  ), "\n")
  invisible(x)
}
