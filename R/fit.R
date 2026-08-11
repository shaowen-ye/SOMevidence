.fit_one_som <- function(data, analysis, spec, grid_row, seed,
                         preprocess, keep_model) {
  n_units <- grid_row$xdim * grid_row$ydim
  if (length(analysis) < n_units) {
    .abort(sprintf(
      "The analysis split has %d rows but the %dx%d grid has %d units.",
      length(analysis), grid_row$xdim, grid_row$ydim, n_units
    ))
  }
  fitted_preprocess <- vector("list", length(data$layers))
  names(fitted_preprocess) <- names(data$layers)
  processed_all <- vector("list", length(data$layers))
  names(processed_all) <- names(data$layers)

  for (nm in names(data$layers)) {
    fitted <- .fit_preprocessor(
      data$layers[[nm]][analysis, , drop = FALSE],
      preprocess[[nm]]
    )
    fitted_preprocess[[nm]] <- fitted$fitted
    processed_all[[nm]] <- .apply_preprocessor(data$layers[[nm]], fitted$fitted)
  }
  training_layers <- lapply(processed_all, function(x) x[analysis, , drop = FALSE])

  if (any(vapply(training_layers, function(x) {
    any(rowMeans(is.na(x)) > spec$max_na_fraction)
  }, logical(1)))) {
    .abort("At least one training row exceeds `max_na_fraction`.")
  }

  grid <- kohonen::somgrid(
    xdim = grid_row$xdim,
    ydim = grid_row$ydim,
    topo = spec$topology,
    neighbourhood.fct = spec$neighbourhood,
    toroidal = spec$toroidal
  )

  common_args <- list(
    grid = grid,
    rlen = spec$rlen,
    alpha = spec$alpha,
    maxNA.fraction = spec$max_na_fraction,
    keep.data = keep_model,
    mode = spec$mode,
    cores = spec$cores
  )
  if (!is.null(spec$radius)) common_args$radius <- spec$radius

  layer_weighting <- .resolve_layer_weights(
    training_layers,
    spec$layer_weights,
    spec$normalize_layers
  )

  model <- .with_reproducible_seed(seed, {
    if (length(training_layers) == 1L) {
      do.call(kohonen::som, c(list(X = training_layers[[1L]]), common_args))
    } else {
      do.call(
        kohonen::supersom,
        c(
          list(
            data = training_layers,
            user.weights = layer_weighting$effective,
            normalizeDataLayers = FALSE
          ),
          common_args
        )
      )
    }
  })

  newdata <- if (length(processed_all) == 1L) processed_all[[1L]] else processed_all
  mapped <- kohonen::map(
    model,
    newdata = newdata,
    maxNA.fraction = spec$max_na_fraction
  )
  train_bmu <- mapped$unit.classif[analysis]
  codes <- model$codes
  if (is.null(names(codes))) names(codes) <- names(training_layers)

  result <- list(
    success = TRUE,
    analysis = analysis,
    fitted_preprocess = fitted_preprocess,
    processed_all = processed_all,
    bmu = mapped$unit.classif,
    distances = mapped$distances,
    training_quantization_error = mean(mapped$distances[analysis], na.rm = TRUE),
    empty_unit_rate = 1 - length(unique(train_bmu[!is.na(train_bmu)])) /
      (grid_row$xdim * grid_row$ydim),
    model = if (keep_model) model else NULL,
    codes = codes,
    grid = model$grid,
    user_weights = model$user.weights,
    distance_weights = model$distance.weights,
    requested_layer_weights = layer_weighting$requested,
    layer_mean_squared_distance = layer_weighting$mean_squared_distance,
    effective_layer_weights = layer_weighting$effective,
    whatmap = model$whatmap
  )
  result$training_topographic_error <- .topographic_error(result, analysis)
  result$processed_all <- NULL
  result
}

#' Fit a reproducible ensemble of self-organizing maps
#'
#' Every model receives preprocessing parameters estimated only from its own
#' analysis rows. Failures are recorded instead of silently discarded.
#' Topographic error is computed during fitting so duplicate full processed
#' matrices need not be retained in every ensemble member.
#'
#' @param data A `som_data` object.
#' @param spec A `som_spec` object.
#' @param resamples A `som_resamples` object. The default fits the full data.
#' @param preprocess One `som_preprocess` object or a named object per layer.
#' @param keep_models Whether to retain fitted `kohonen` objects.
#' @param fail_fast Whether to stop at the first model failure.
#' @param parallel Whether to distribute ensemble members through the current
#'   `future` plan using `future.apply`. The package never changes that plan.
#'   Explicit model seeds retain reproducibility across sequential and future
#'   execution. Avoid combining worker-level parallelism with `spec$cores > 1`
#'   unless nested parallelism is intended.
#'
#' @return A `som_ensemble` object.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 1)
#' specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
#' ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
#' ensemble
#' @export
fit_som_ensemble <- function(data, spec, resamples = NULL,
                             preprocess = som_preprocess(),
                             keep_models = TRUE, fail_fast = FALSE,
                             parallel = FALSE) {
  if (!inherits(data, "som_data")) .abort("`data` must come from `som_data()`.")
  if (!inherits(spec, "som_spec")) .abort("`spec` must come from `som_spec()`.")
  .assert_flag(keep_models, "keep_models")
  .assert_flag(fail_fast, "fail_fast")
  .assert_flag(parallel, "parallel")
  if (parallel && !requireNamespace("future.apply", quietly = TRUE)) {
    .abort("Install the suggested package `future.apply` for parallel fitting.")
  }

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
  } else if (!identical(
    as.character(resamples$sample_ids),
    as.character(data$metadata$id)
  )) {
    .abort(paste0(
      "`resamples` were created for different sample IDs or row order. ",
      "Recreate them from the current `som_data` object."
    ))
  }
  preprocess <- .normalise_preprocess(preprocess, names(data$layers))
  budget <- expand_som_spec(spec)

  n_budget <- nrow(budget)
  jobs <- seq_len(length(resamples$splits) * n_budget)

  fit_job <- function(task_index) {
    split_index <- ((task_index - 1L) %/% n_budget) + 1L
    budget_index <- ((task_index - 1L) %% n_budget) + 1L
    split <- resamples$splits[[split_index]]
    job <- budget[budget_index, , drop = FALSE]
    captured <- .capture_warnings(
      .fit_one_som(
        data = data,
        analysis = split$analysis,
        spec = spec,
        grid_row = job,
        seed = job$seed,
        preprocess = preprocess,
        keep_model = keep_models
      )
    )
    fitted <- if (inherits(captured$value, "error")) {
      if (fail_fast) stop(captured$value)
      list(success = FALSE, error = conditionMessage(captured$value))
    } else {
      captured$value
    }
    fitted$warnings <- captured$warnings
    fitted$id <- paste(split$id, job$model_id, sep = "__")
    fitted$split_id <- split$id
    fitted$assessment <- split$assessment
    fitted$grid_id <- job$grid_id
    fitted$xdim <- job$xdim
    fitted$ydim <- job$ydim
    fitted$seed <- job$seed
    fitted
  }
  fits <- if (parallel) {
    future.apply::future_lapply(jobs, fit_job, future.seed = TRUE)
  } else {
    lapply(jobs, fit_job)
  }

  failures <- lapply(
    Filter(function(fit) !isTRUE(fit$success), fits),
    function(fit) {
      data.frame(
        id = fit$id,
        split_id = fit$split_id,
        grid_id = fit$grid_id,
        seed = fit$seed,
        error = fit$error,
        stringsAsFactors = FALSE
      )
    }
  )

  failure_table <- if (length(failures)) {
    do.call(rbind, failures)
  } else {
    data.frame(
      id = character(), split_id = character(), grid_id = integer(),
      seed = integer(), error = character(), stringsAsFactors = FALSE
    )
  }

  warnings <- lapply(fits, function(fit) {
    if (is.null(fit$warnings) || !nrow(fit$warnings)) return(NULL)
    cbind(
      data.frame(
        id = fit$id,
        split_id = fit$split_id,
        grid_id = fit$grid_id,
        seed = fit$seed,
        stringsAsFactors = FALSE
      ),
      fit$warnings
    )
  })
  warnings <- Filter(Negate(is.null), warnings)
  warning_table <- if (length(warnings)) {
    do.call(rbind, warnings)
  } else {
    data.frame(
      id = character(), split_id = character(), grid_id = integer(),
      seed = integer(), warning_class = character(), warning = character(),
      stringsAsFactors = FALSE
    )
  }

  .new_som_object(
    list(
      data = data,
      spec = spec,
      resamples = resamples,
      preprocess = preprocess,
      fits = fits,
      failures = failure_table,
      warnings = warning_table,
      expected_models = length(fits),
      parallel = parallel
    ),
    "som_ensemble"
  )
}

#' @export
print.som_ensemble <- function(x, ...) {
  successes <- sum(vapply(x$fits, function(z) isTRUE(z$success), logical(1)))
  cat("<som_ensemble>\n")
  cat("  attempted:", x$expected_models, "\n")
  cat("  succeeded:", successes, "\n")
  cat("  failed   :", nrow(x$failures), "\n")
  cat("  warnings :", nrow(x$warnings), "\n")
  cat("  samples  :", nrow(x$data$metadata), "\n")
  invisible(x)
}
