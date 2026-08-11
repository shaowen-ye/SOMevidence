.validate_mapping_layers <- function(ensemble, new_data) {
  expected_layers <- names(ensemble$data$layers)
  if (!setequal(names(new_data$layers), expected_layers)) {
    .abort("New data must contain the same named layers as the fitted ensemble.")
  }
  output <- new_data$layers[expected_layers]
  for (nm in expected_layers) {
    expected <- colnames(ensemble$data$layers[[nm]])
    observed <- colnames(output[[nm]])
    if (!setequal(observed, expected)) {
      .abort(sprintf(
        "New-data variables in layer `%s` do not match the fitted ensemble.",
        nm
      ))
    }
    output[[nm]] <- output[[nm]][, expected, drop = FALSE]
  }
  output
}

#' Map new observations through a fitted SOM ensemble
#'
#' Every ensemble member applies only the transformations, centering, scaling
#' and layer geometry learned from its own analysis split. The function reports
#' best-matching units and mapping distances separately by fit; it does not
#' align nodes across different grids or call the result prediction accuracy.
#'
#' @param ensemble A `som_ensemble` fitted with `keep_models = TRUE`.
#' @param new_data A `som_data` object with the same named layers and variables
#'   as the training data. Metadata may differ and is retained.
#' @param fail_fast Whether to stop at the first member-specific mapping error.
#'
#' @return A `som_newdata_mapping` object with sample-by-fit records, fit-level
#'   distance and occupancy summaries, failures and new-data metadata.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 8)
#' specification <- som_spec(c(3, 2), seeds = 1, rlen = 10, k = 2)
#' ensemble <- fit_som_ensemble(data, specification, keep_models = TRUE)
#' new_data <- som_data(layers = list(
#'   environment = data$layers$environment[1:3, , drop = FALSE]
#' ))
#' map_som_ensemble(ensemble, new_data)
#' @export
map_som_ensemble <- function(ensemble, new_data, fail_fast = FALSE) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`ensemble` must come from `fit_som_ensemble()`.")
  }
  if (!inherits(new_data, "som_data")) {
    .abort("`new_data` must come from `som_data()`.")
  }
  .assert_flag(fail_fast, "fail_fast")
  new_layers <- .validate_mapping_layers(ensemble, new_data)
  successful <- Filter(function(fit) isTRUE(fit$success), ensemble$fits)
  if (!length(successful)) .abort("No successful SOM fit is available for mapping.")
  if (any(vapply(successful, function(fit) is.null(fit$model), logical(1)))) {
    .abort("Refit the ensemble with `keep_models = TRUE` before mapping new data.")
  }

  mapped <- lapply(successful, function(fit) {
    captured <- .capture_warnings({
      processed <- lapply(names(new_layers), function(nm) {
        .apply_preprocessor(new_layers[[nm]], fit$fitted_preprocess[[nm]])
      })
      names(processed) <- names(new_layers)
      payload <- if (length(processed) == 1L) processed[[1L]] else processed
      mapping <- kohonen::map(
        fit$model,
        newdata = payload,
        maxNA.fraction = ensemble$spec$max_na_fraction
      )
      training_units <- unique(fit$bmu[fit$analysis])
      training_median <- stats::median(fit$distances[fit$analysis], na.rm = TRUE)
      new_median <- stats::median(mapping$distances, na.rm = TRUE)
      list(
        bmu = mapping$unit.classif,
        distances = mapping$distances,
        training_median = training_median,
        new_median = new_median,
        distance_ratio = if (is.finite(training_median) && training_median > 0) {
          new_median / training_median
        } else {
          NA_real_
        },
        unoccupied_unit_rate = .unoccupied_rate(
          mapping$unit.classif, training_units
        )
      )
    })
    if (inherits(captured$value, "error")) {
      if (fail_fast) stop(captured$value)
      list(
        success = FALSE, fit = fit,
        error = conditionMessage(captured$value),
        warnings = captured$warnings
      )
    } else {
      c(
        list(success = TRUE, fit = fit, warnings = captured$warnings),
        captured$value
      )
    }
  })

  successful_maps <- Filter(function(result) isTRUE(result$success), mapped)
  records <- if (length(successful_maps)) {
    do.call(rbind, lapply(successful_maps, function(result) {
      data.frame(
        fit_id = result$fit$id,
        sample_id = new_data$metadata$id,
        bmu = result$bmu,
        distance = result$distances,
        grid_id = result$fit$grid_id,
        xdim = result$fit$xdim,
        ydim = result$fit$ydim,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      fit_id = character(), sample_id = character(), bmu = integer(),
      distance = numeric(), grid_id = integer(), xdim = integer(),
      ydim = integer(), stringsAsFactors = FALSE
    )
  }
  summary <- if (length(successful_maps)) {
    do.call(rbind, lapply(successful_maps, function(result) {
      mapped_rows <- !is.na(result$bmu) & is.finite(result$distances)
      data.frame(
        fit_id = result$fit$id,
        grid_id = result$fit$grid_id,
        xdim = result$fit$xdim,
        ydim = result$fit$ydim,
        n_new = length(result$bmu),
        n_mapped = sum(mapped_rows),
        mapping_coverage = mean(mapped_rows),
        median_training_distance = result$training_median,
        median_new_distance = result$new_median,
        distance_ratio = result$distance_ratio,
        unoccupied_unit_rate = result$unoccupied_unit_rate,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      fit_id = character(), grid_id = integer(), xdim = integer(),
      ydim = integer(), n_new = integer(), n_mapped = integer(),
      mapping_coverage = numeric(),
      median_training_distance = numeric(), median_new_distance = numeric(),
      distance_ratio = numeric(), unoccupied_unit_rate = numeric(),
      stringsAsFactors = FALSE
    )
  }
  failed_maps <- Filter(function(result) !isTRUE(result$success), mapped)
  failures <- if (length(failed_maps)) {
    do.call(rbind, lapply(failed_maps, function(result) {
      data.frame(
        fit_id = result$fit$id,
        error = result$error,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(fit_id = character(), error = character())
  }
  warning_rows <- Filter(function(result) nrow(result$warnings) > 0L, mapped)
  warnings <- if (length(warning_rows)) {
    do.call(rbind, lapply(warning_rows, function(result) {
      cbind(
        data.frame(fit_id = result$fit$id, stringsAsFactors = FALSE),
        result$warnings
      )
    }))
  } else {
    data.frame(
      fit_id = character(), warning_class = character(),
      warning = character(), stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      records = records,
      summary = summary,
      failures = failures,
      warnings = warnings,
      metadata = new_data$metadata,
      n_expected_fits = length(successful)
    ),
    class = "som_newdata_mapping"
  )
}

#' @export
print.som_newdata_mapping <- function(x, ...) {
  cat("<som_newdata_mapping>\n")
  cat("  new samples    :", nrow(x$metadata), "\n")
  cat("  fits attempted :", x$n_expected_fits, "\n")
  cat("  fits mapped    :", nrow(x$summary), "\n")
  cat("  fits failed    :", nrow(x$failures), "\n")
  cat("  warnings       :", nrow(x$warnings %||% data.frame()), "\n")
  cat("  node consensus : not computed across grids\n")
  invisible(x)
}
