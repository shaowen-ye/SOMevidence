.reference_matrix <- function(
  data,
  analysis,
  preprocess,
  layer_weights,
  normalize_layers
) {
  preprocess <- .normalise_preprocess(preprocess, names(data$layers))
  processed <- vector("list", length(data$layers))
  names(processed) <- names(data$layers)
  fitted <- vector("list", length(data$layers))
  names(fitted) <- names(data$layers)

  for (nm in names(data$layers)) {
    learned <- .fit_preprocessor(
      data$layers[[nm]][analysis, , drop = FALSE], preprocess[[nm]]
    )
    fitted[[nm]] <- learned$fitted
    processed[[nm]] <- .apply_preprocessor(data$layers[[nm]], learned$fitted)
  }
  training_layers <- lapply(processed, function(x) {
    x[analysis, , drop = FALSE]
  })
  if (any(vapply(training_layers, anyNA, logical(1)))) {
    .abort("Cross-model methods require complete analysis-row predictors.")
  }
  weighting <- .resolve_layer_weights(
    training_layers,
    layer_weights,
    normalize_layers
  )
  weighted <- Map(function(x, weight) {
    x * sqrt(weight)
  }, processed, weighting$effective)
  list(
    matrix = do.call(cbind, weighted),
    fitted_preprocess = fitted,
    requested_layer_weights = weighting$requested,
    layer_mean_squared_distance = weighting$mean_squared_distance,
    effective_layer_weights = weighting$effective
  )
}

.fit_ward_tree <- function(training) {
  stats::hclust(stats::dist(training), method = "ward.D2")
}

.nearest_centroid <- function(x, centres) {
  distance <- vapply(seq_len(nrow(centres)), function(i) {
    rowSums(sweep(x, 2L, centres[i, ], "-")^2)
  }, numeric(nrow(x)))
  max.col(-distance, ties.method = "first")
}

.fit_cross_partition <- function(x, analysis, method, k, seed,
                                 kmeans_nstart, kmeans_iter_max,
                                 gmm_model_names, ward_tree = NULL) {
  training <- x[analysis, , drop = FALSE]
  if (anyNA(training)) {
    .abort("Cross-model training rows must be complete.")
  }
  if (nrow(training) <= k) {
    .abort("A cross-model fit requires more analysis rows than clusters.")
  }
  predictable <- stats::complete.cases(x)
  labels <- rep(NA_integer_, nrow(x))

  if (method == "kmeans") {
    model <- .with_reproducible_seed(seed, stats::kmeans(
      training,
      centers = k, nstart = kmeans_nstart,
      iter.max = kmeans_iter_max,
      algorithm = "Hartigan-Wong"
    ))
    labels[analysis] <- model$cluster
    projection_rows <- setdiff(which(predictable), analysis)
    if (length(projection_rows)) {
      labels[projection_rows] <- .nearest_centroid(
        x[projection_rows, , drop = FALSE], model$centers
      )
    }
    selected_model <- "Hartigan-Wong"
    prediction_rule <- "training assignment; nearest-centroid projection"
  } else if (method == "ward") {
    tree <- ward_tree %||% .fit_ward_tree(training)
    training_labels <- stats::cutree(tree, k = k)
    centres <- do.call(rbind, lapply(seq_len(k), function(cluster) {
      colMeans(training[training_labels == cluster, , drop = FALSE])
    }))
    labels[analysis] <- training_labels
    projection_rows <- setdiff(which(predictable), analysis)
    if (length(projection_rows)) {
      labels[projection_rows] <- .nearest_centroid(
        x[projection_rows, , drop = FALSE], centres
      )
    }
    model <- list(tree = tree, centres = centres)
    selected_model <- "ward.D2"
    prediction_rule <- "Ward.D2 cutree; nearest-centroid projection"
  } else {
    if (!requireNamespace("mclust", quietly = TRUE)) {
      .abort("Package `mclust` is required for Gaussian mixture models.")
    }
    # Mclust evaluates a reconstructed mclustBIC call in the caller. Provide
    # the namespaced function locally so optional use does not require
    # attaching the whole mclust package to the search path.
    assign("mclustBIC", mclust::mclustBIC) # nolint: object_name_linter.
    model <- .with_reproducible_seed(seed, mclust::Mclust(
      training, G = k, modelNames = gmm_model_names, verbose = FALSE
    ))
    if (is.null(model$classification) || model$G != k) {
      .abort("The Gaussian mixture model did not return the requested partition.")
    }
    labels[analysis] <- model$classification
    projection_rows <- setdiff(which(predictable), analysis)
    if (length(projection_rows)) {
      predicted <- stats::predict(
        model, newdata = x[projection_rows, , drop = FALSE]
      )
      labels[projection_rows] <- predicted$classification
    }
    selected_model <- model$modelName
    prediction_rule <- "model classification; posterior projection"
  }
  list(
    sample_labels = as.integer(labels),
    selected_model = selected_model,
    prediction_rule = prediction_rule,
    model = model
  )
}

#' Fit controlled cross-model reference partitions
#'
#' K-means, Ward.D2 and Gaussian mixture models use the same data object,
#' resampling indices and leakage-safe preprocessing as a fitted SOM ensemble.
#' The function creates reference partitions for triangulation; it does not
#' rank algorithms or define any method as ground truth.
#'
#' @param ensemble A fitted `som_ensemble` defining data, preprocessing and
#'   resampling.
#' @param methods Any of `"kmeans"`, `"ward"` and `"gmm"`.
#' @param k Candidate numbers of clusters.
#' @param kmeans_seeds Random seeds used only for K-means initialization.
#' @param kmeans_nstart Number of random starts per K-means fit.
#' @param kmeans_iter_max Maximum number of iterations per K-means start.
#' @param gmm_model_names Optional covariance models passed to
#'   [mclust::Mclust()]. BIC selection occurs within each analysis split.
#' @param keep_models Whether to retain fitted cross-model objects.
#' @param fail_fast Whether to stop at the first fit failure.
#' @param gmm_seed Base random seed for reproducible GMM initialization. A
#'   deterministic fit-specific seed is derived from the split and candidate
#'   `k`, and is retained in the result object.
#'
#' @return A `som_cross_models` object with partitions, warnings and explicit
#'   failures.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 4)
#' specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
#' ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
#' fit_cross_models(ensemble, methods = c("kmeans", "ward"))
#' @export
fit_cross_models <- function(
  ensemble,
  methods = c("kmeans", "ward"),
  k = ensemble$spec$k,
  kmeans_seeds = 1L,
  kmeans_nstart = 50L,
  kmeans_iter_max = 100L,
  gmm_model_names = NULL,
  keep_models = FALSE,
  fail_fast = FALSE,
  gmm_seed = 1L
) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`ensemble` must come from `fit_som_ensemble()`.")
  }
  methods <- unique(match.arg(methods, c("kmeans", "ward", "gmm"),
    several.ok = TRUE
  ))
  if ("gmm" %in% methods && !requireNamespace("mclust", quietly = TRUE)) {
    .abort(paste0(
      "Method `gmm` requires the suggested package `mclust`; install it ",
      "or request only `kmeans` and/or `ward`."
    ))
  }
  .assert_integer_vector(k, "k", lower = 2)
  .assert_integer_vector(kmeans_seeds, "kmeans_seeds", lower = 0)
  .assert_scalar_integer(kmeans_nstart, "kmeans_nstart", lower = 1)
  .assert_scalar_integer(kmeans_iter_max, "kmeans_iter_max", lower = 1)
  .assert_scalar_integer(gmm_seed, "gmm_seed", lower = 0)
  .assert_flag(keep_models, "keep_models")
  .assert_flag(fail_fast, "fail_fast")

  records <- list()
  failures <- list()
  warnings <- list()
  record_cursor <- 0L
  failure_cursor <- 0L
  warning_cursor <- 0L
  candidate_k_values <- sort(unique(as.integer(k)))
  for (split in ensemble$resamples$splits) {
    prepared <- tryCatch(
      .reference_matrix(
        ensemble$data, split$analysis, ensemble$preprocess,
        ensemble$spec$layer_weights, ensemble$spec$normalize_layers
      ),
      error = function(e) e
    )
    ward_tree_capture <- NULL
    for (method in methods) {
      seeds <- if (method == "kmeans") {
        as.integer(kmeans_seeds)
      } else {
        NA_integer_
      }
      for (candidate_k in candidate_k_values) {
        if (
          method == "ward" &&
            !inherits(prepared, "error") &&
            length(split$analysis) > candidate_k &&
            is.null(ward_tree_capture)
        ) {
          training <- prepared$matrix[split$analysis, , drop = FALSE]
          ward_tree_capture <- .capture_warnings(.fit_ward_tree(training))
        }
        if (method == "gmm") {
          seeds <- .seed_from_key(gmm_seed, split$id, paste0("k", candidate_k))
        }
        for (seed in seeds) {
          id <- paste(
            split$id, method, paste0("k", candidate_k),
            if (method %in% c("kmeans", "gmm")) paste0("s", seed) else NULL,
            sep = "__"
          )
          use_ward_tree <- method == "ward" &&
            !inherits(prepared, "error") &&
            length(split$analysis) > candidate_k &&
            !is.null(ward_tree_capture)
          captured <- if (
            use_ward_tree && inherits(ward_tree_capture$value, "error")
          ) {
            ward_tree_capture
          } else {
            task_capture <- .capture_warnings({
              if (inherits(prepared, "error")) stop(prepared)
              .fit_cross_partition(
                prepared$matrix, split$analysis, method, candidate_k, seed,
                as.integer(kmeans_nstart), as.integer(kmeans_iter_max),
                gmm_model_names,
                ward_tree = if (use_ward_tree) {
                  ward_tree_capture$value
                } else {
                  NULL
                }
              )
            })
            if (use_ward_tree && nrow(ward_tree_capture$warnings)) {
              task_capture$warnings <- rbind(
                ward_tree_capture$warnings,
                task_capture$warnings
              )
            }
            task_capture
          }
          fitted <- captured$value
          if (nrow(captured$warnings)) {
            for (warning_index in seq_len(nrow(captured$warnings))) {
              warning_cursor <- warning_cursor + 1L
              warnings[[warning_cursor]] <- data.frame(
                id = id,
                split_id = split$id,
                method = method,
                k = candidate_k,
                seed = seed,
                warning_class = captured$warnings$warning_class[[warning_index]],
                warning = captured$warnings$warning[[warning_index]],
                stringsAsFactors = FALSE
              )
            }
          }
          if (inherits(fitted, "error")) {
            if (fail_fast) stop(fitted)
            failure_cursor <- failure_cursor + 1L
            failures[[failure_cursor]] <- data.frame(
              id = id, split_id = split$id, method = method,
              k = candidate_k, seed = seed,
              error = conditionMessage(fitted), stringsAsFactors = FALSE
            )
          } else {
            record_cursor <- record_cursor + 1L
            records[[record_cursor]] <- list(
              id = id,
              split_id = split$id,
              method = method,
              k = candidate_k,
              seed = seed,
              selected_model = fitted$selected_model,
              prediction_rule = fitted$prediction_rule,
              sample_labels = fitted$sample_labels,
              analysis = split$analysis,
              assessment = split$assessment,
              fitted_preprocess = prepared$fitted_preprocess,
              requested_layer_weights = prepared$requested_layer_weights,
              layer_mean_squared_distance = prepared$layer_mean_squared_distance,
              effective_layer_weights = prepared$effective_layer_weights,
              model = if (keep_models) fitted$model else NULL
            )
          }
        }
      }
    }
  }
  failure_table <- if (length(failures)) {
    do.call(rbind, failures)
  } else {
    data.frame(
      id = character(), split_id = character(), method = character(),
      k = integer(), seed = integer(), error = character(),
      stringsAsFactors = FALSE
    )
  }
  warning_table <- if (length(warnings)) {
    do.call(rbind, warnings)
  } else {
    data.frame(
      id = character(), split_id = character(), method = character(),
      k = integer(), seed = integer(), warning_class = character(),
      warning = character(), stringsAsFactors = FALSE
    )
  }
  .new_som_object(
    list(
      records = records,
      failures = failure_table,
      warnings = warning_table,
      methods = methods,
      k = sort(unique(as.integer(k))),
      ensemble = ensemble
    ),
    "som_cross_models"
  )
}

#' Compare SOM partitions with controlled cross-model references
#'
#' @param partitions A `som_partitions` object.
#' @param cross_models A `som_cross_models` object.
#' @param scope Compare analysis rows (`"analysis"`), assessment rows when
#'   available (`"assessment"`) or all mapped rows (`"all"`). The analysis
#'   scope is the default for hard-partition defensibility; assessment scope is
#'   a separate mapping-transfer diagnostic.
#'
#' @return A `som_cross_comparison` containing ARI and AMI effect sizes,
#'   summaries, reference-fit success rates, source-partition completeness and
#'   source-partition provenance. Neither agreement metric is reported as
#'   accuracy.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 5)
#' specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
#' ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
#' partitions <- partition_som(ensemble)
#' references <- fit_cross_models(ensemble, methods = "ward")
#' compare_cross_models(partitions, references)
#' @export
compare_cross_models <- function(
  partitions, cross_models,
  scope = c("analysis", "assessment", "all")
) {
  if (!inherits(partitions, "som_partitions")) {
    .abort("`partitions` must come from `partition_som()`.")
  }
  if (!inherits(cross_models, "som_cross_models")) {
    .abort("`cross_models` must come from `fit_cross_models()`.")
  }
  if (!identical(partitions$ensemble, cross_models$ensemble)) {
    .abort("`partitions` and `cross_models` must share the same source ensemble.")
  }
  scope <- match.arg(scope)
  if (scope == "assessment" && any(vapply(
    partitions$records,
    function(record) !length(record$assessment),
    logical(1)
  ))) {
    .abort(paste0(
      "Assessment-scope comparison requires non-empty assessment rows in ",
      "every SOM partition; no analysis fallback is used."
    ))
  }
  rows <- list()
  cursor <- 0L
  for (som in partitions$records) {
    references <- Filter(function(reference) {
      reference$split_id == som$split_id && reference$k == som$k
    }, cross_models$records)
    for (reference in references) {
      som_labels <- som$mapped_labels %||% som$sample_labels
      index <- if (scope == "all") {
        seq_along(som_labels)
      } else if (scope == "analysis") {
        som$analysis
      } else {
        som$assessment
      }
      cursor <- cursor + 1L
      jointly_observed <- !is.na(som_labels[index]) &
        !is.na(reference$sample_labels[index])
      agreement <- .partition_agreement(
        som_labels[index],
        reference$sample_labels[index]
      )
      rows[[cursor]] <- data.frame(
        split_id = som$split_id,
        k = som$k,
        som_fit = som$id,
        reference_fit = reference$id,
        method = reference$method,
        selected_model = reference$selected_model,
        scope = scope,
        n = sum(jointly_observed),
        ari = agreement[["ari"]],
        ami = agreement[["ami"]],
        stringsAsFactors = FALSE
      )
    }
  }
  comparisons <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      split_id = character(), k = integer(), som_fit = character(),
      reference_fit = character(), method = character(),
      selected_model = character(), scope = character(), n = integer(),
      ari = numeric(), ami = numeric(), stringsAsFactors = FALSE
    )
  }
  summary <- if (nrow(comparisons)) {
    key <- interaction(
      comparisons$scope, comparisons$method, comparisons$k, drop = TRUE
    )
    do.call(rbind, lapply(split(comparisons, key), function(x) {
      ari_interval <- .quantile_safe(x$ari, c(0.025, 0.975))
      ami_interval <- .quantile_safe(x$ami, c(0.025, 0.975))
      data.frame(
        scope = x$scope[[1L]], method = x$method[[1L]],
        k = x$k[[1L]], n_comparisons = nrow(x),
        median_ari = stats::median(x$ari, na.rm = TRUE),
        ari_q025 = ari_interval[[1L]], ari_q975 = ari_interval[[2L]],
        median_ami = stats::median(x$ami, na.rm = TRUE),
        ami_q025 = ami_interval[[1L]], ami_q975 = ami_interval[[2L]],
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      scope = character(), method = character(), k = integer(),
      n_comparisons = integer(),
      median_ari = numeric(), ari_q025 = numeric(), ari_q975 = numeric(),
      median_ami = numeric(), ami_q025 = numeric(), ami_q975 = numeric()
    )
  }
  rownames(summary) <- NULL
  reference_grid <- expand.grid(
    method = cross_models$methods,
    k = cross_models$k,
    stringsAsFactors = FALSE
  )
  reference_status <- do.call(rbind, lapply(
    seq_len(nrow(reference_grid)),
    function(i) {
      method <- reference_grid$method[[i]]
      candidate_k <- reference_grid$k[[i]]
      succeeded <- sum(vapply(cross_models$records, function(record) {
        identical(record$method, method) && record$k == candidate_k
      }, logical(1)))
      failed <- sum(
        cross_models$failures$method == method &
          cross_models$failures$k == candidate_k
      )
      expected <- succeeded + failed
      data.frame(
        method = method,
        k = candidate_k,
        n_expected = expected,
        n_succeeded = succeeded,
        n_failed = failed,
        success_rate = if (expected) succeeded / expected else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  ))
  rownames(reference_status) <- NULL
  completeness_columns <- intersect(
    c(
      "k", "n_partitions", "n_complete_partitions",
      "min_observed_clusters"
    ),
    names(partitions$stability)
  )
  partition_completeness <- partitions$stability[
    , completeness_columns, drop = FALSE
  ]
  .new_som_object(
    list(
      comparisons = comparisons,
      summary = summary,
      failures = cross_models$failures,
      warnings = cross_models$warnings,
      methods = cross_models$methods,
      reference_status = reference_status,
      partition_completeness = partition_completeness,
      partition_method = partitions$partition_method %||% partitions$method,
      partition_records = partitions$records,
      scope = scope,
      ensemble = partitions$ensemble
    ),
    "som_cross_comparison"
  )
}

#' @export
print.som_cross_models <- function(x, ...) {
  cat("<som_cross_models>\n")
  cat("  successful fits:", length(x$records), "\n")
  cat("  failed fits    :", nrow(x$failures), "\n")
  cat("  warnings       :", nrow(x$warnings), "\n")
  cat("  methods        :", paste(x$methods, collapse = ", "), "\n")
  invisible(x)
}

#' @export
print.som_cross_comparison <- function(x, ...) {
  cat("<som_cross_comparison>\n")
  cat("  comparisons:", nrow(x$comparisons), "\n")
  cat("  scope      :", x$scope, "\n")
  cat("  metrics    : ARI and AMI (agreement, not accuracy)\n")
  invisible(x)
}
