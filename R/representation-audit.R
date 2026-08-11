.validate_representation_fit <- function(fit, n, layers, splits) {
  required <- c(
    "id", "split_id", "grid_id", "xdim", "ydim", "seed", "analysis",
    "assessment", "bmu", "distances", "codes", "grid", "user_weights",
    "distance_weights", "fitted_preprocess"
  )
  missing_fields <- setdiff(required, names(fit))
  if (length(missing_fields)) {
    .abort(paste0(
      "A successful ensemble fit lacks representation fields: ",
      paste(missing_fields, collapse = ", "), "."
    ))
  }
  if (length(fit$bmu) != n || length(fit$distances) != n) {
    .abort(sprintf("Fit `%s` does not map every ensemble sample.", fit$id))
  }
  validate_rows <- function(rows, name) {
    if (!is.numeric(rows) || anyNA(rows) || any(rows %% 1 != 0) ||
          any(rows < 1L | rows > n) || anyDuplicated(rows)) {
      .abort(sprintf("Fit `%s` has invalid `%s` rows.", fit$id, name))
    }
    as.integer(rows)
  }
  fit$analysis <- validate_rows(fit$analysis, "analysis")
  fit$assessment <- validate_rows(fit$assessment, "assessment")
  if (length(intersect(fit$analysis, fit$assessment))) {
    .abort(sprintf("Fit `%s` has overlapping analysis and assessment rows.", fit$id))
  }
  if (!is.character(fit$id) || length(fit$id) != 1L || is.na(fit$id) ||
        !nzchar(fit$id) || !is.character(fit$split_id) ||
        length(fit$split_id) != 1L || is.na(fit$split_id) ||
        !nzchar(fit$split_id)) {
    .abort("Every successful representation fit needs valid fit and split IDs.")
  }
  split_ids <- vapply(splits, `[[`, character(1), "id")
  split_index <- match(fit$split_id, split_ids)
  if (is.na(split_index)) {
    .abort(sprintf(
      "Fit `%s` does not match an originating resampling split.", fit$id
    ))
  }
  source_split <- splits[[split_index]]
  if (!identical(as.integer(fit$analysis), as.integer(source_split$analysis)) ||
    !identical(
      as.integer(fit$assessment), as.integer(source_split$assessment)
    )) {
    .abort(sprintf(
      "Fit `%s` rows do not match its originating resampling split.", fit$id
    ))
  }
  layer_names <- names(layers)
  if (!is.list(fit$codes) || !length(fit$codes) ||
        !identical(names(fit$codes), layer_names) ||
        !is.list(fit$fitted_preprocess) ||
        !identical(names(fit$fitted_preprocess), layer_names)) {
    .abort(sprintf(
      "Fit `%s` has incompatible codebook preprocessing fields.", fit$id
    ))
  }
  valid_codebooks <- vapply(seq_along(fit$codes), function(i) {
    codebook <- fit$codes[[i]]
    is.matrix(codebook) && is.numeric(codebook) &&
      nrow(codebook) >= 2L && ncol(codebook) == ncol(layers[[i]]) &&
      all(is.finite(codebook))
  }, logical(1))
  if (!all(valid_codebooks)) {
    .abort(sprintf("Fit `%s` contains an invalid codebook.", fit$id))
  }
  codebook_units <- vapply(fit$codes, nrow, integer(1))
  if (length(unique(codebook_units)) != 1L) {
    .abort(sprintf("Fit `%s` codebooks use inconsistent unit counts.", fit$id))
  }
  units <- codebook_units[[1L]]
  grid <- fit$grid
  valid_dimension <- function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value %% 1 == 0 && value >= 1L
  }
  if (!inherits(grid, "somgrid") ||
        !valid_dimension(fit$xdim) || !valid_dimension(fit$ydim) ||
        !valid_dimension(grid$xdim) || !valid_dimension(grid$ydim) ||
        fit$xdim != grid$xdim || fit$ydim != grid$ydim ||
        fit$xdim * fit$ydim != units || !is.matrix(grid$pts) ||
        !is.numeric(grid$pts) || !identical(dim(grid$pts), c(units, 2L)) ||
        any(!is.finite(grid$pts)) || anyDuplicated(data.frame(grid$pts))) {
    .abort(sprintf(
      "Fit `%s` has inconsistent codebook and grid dimensions.", fit$id
    ))
  }
  if (!is.character(grid$topo) || length(grid$topo) != 1L ||
        is.na(grid$topo) ||
        !grid$topo %in% c("rectangular", "hexagonal")) {
    .abort(sprintf(
      "Fit `%s` grid topology must be rectangular or hexagonal.", fit$id
    ))
  }
  if (!is.logical(grid$toroidal) || length(grid$toroidal) != 1L ||
        is.na(grid$toroidal)) {
    .abort(sprintf(
      "Fit `%s` grid `toroidal` flag must be TRUE or FALSE.", fit$id
    ))
  }
  if (!is.numeric(fit$bmu) || any(
    !is.na(fit$bmu) & (
      !is.finite(fit$bmu) | fit$bmu %% 1 != 0 |
        fit$bmu < 1L | fit$bmu > units
    )
  )) {
    .abort(sprintf("Fit `%s` contains invalid BMU indices.", fit$id))
  }
  if (!is.numeric(fit$distances) || any(
    !is.na(fit$distances) & (
      !is.finite(fit$distances) | fit$distances < 0
    )
  )) {
    .abort(sprintf("Fit `%s` contains invalid mapping distances.", fit$id))
  }
  if (!is.numeric(fit$user_weights) ||
        !is.numeric(fit$distance_weights) ||
        length(fit$user_weights) != length(fit$codes) ||
        length(fit$distance_weights) != length(fit$codes) ||
        anyNA(fit$user_weights) || anyNA(fit$distance_weights) ||
        any(!is.finite(fit$user_weights)) ||
        any(!is.finite(fit$distance_weights)) ||
        any(fit$user_weights < 0) || any(fit$distance_weights < 0)) {
    .abort(sprintf("Fit `%s` has incompatible layer weights.", fit$id))
  }
  effective_weights <- fit$user_weights * fit$distance_weights
  if (any(!is.finite(effective_weights)) ||
        !is.finite(sum(effective_weights)) || sum(effective_weights) <= 0) {
    .abort(sprintf("Fit `%s` has incompatible layer weights.", fit$id))
  }
  invisible(fit)
}

.representation_scope_rows <- function(fit, scope) {
  if (scope == "analysis") fit$analysis else fit$assessment
}

.representation_mapped <- function(fit, rows) {
  !is.na(fit$bmu[rows]) & is.finite(fit$distances[rows])
}

.representation_te <- function(ensemble, fit, rows) {
  if (!length(rows)) return(NA_real_)
  processed <- lapply(names(ensemble$data$layers), function(layer) {
    .apply_preprocessor(
      ensemble$data$layers[[layer]][rows, , drop = FALSE],
      fit$fitted_preprocess[[layer]]
    )
  })
  names(processed) <- names(ensemble$data$layers)
  temporary <- fit
  temporary$processed_all <- processed
  .topographic_error(temporary, seq_along(rows))
}

.som_grid_key <- function(grid) {
  paste(
    grid$xdim, grid$ydim, grid$topo, isTRUE(grid$toroidal),
    paste(formatC(as.vector(grid$pts), digits = 12L), collapse = ","),
    sep = "|"
  )
}

.som_grid_hop_distances <- function(grid) {
  adjacency <- .som_grid_adjacency(grid)
  n_units <- nrow(adjacency)
  hops <- matrix(Inf, nrow = n_units, ncol = n_units)
  diag(hops) <- 0
  for (source in seq_len(n_units)) {
    queue <- source
    cursor <- 1L
    while (cursor <= length(queue)) {
      node <- queue[[cursor]]
      cursor <- cursor + 1L
      neighbours <- which(adjacency[node, ])
      unseen <- neighbours[!is.finite(hops[source, neighbours])]
      if (length(unseen)) {
        hops[source, unseen] <- hops[source, node] + 1
        queue <- c(queue, unseen)
      }
    }
  }
  if (any(!is.finite(hops))) {
    .abort("The SOM grid adjacency graph is disconnected.")
  }
  storage.mode(hops) <- "integer"
  hops
}

.normalise_repr_pairs <- function(pairs, fit_ids) {
  if (is.null(pairs)) {
    if (length(fit_ids) < 2L) {
      return(data.frame(
        fit_a = character(), fit_b = character(), stringsAsFactors = FALSE
      ))
    }
    pairs <- t(utils::combn(fit_ids, 2L))
    return(data.frame(
      fit_a = pairs[, 1L], fit_b = pairs[, 2L], stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(pairs) ||
        !all(c("fit_a", "fit_b") %in% names(pairs))) {
    .abort("`pairs` must be a data frame with `fit_a` and `fit_b` columns.")
  }
  output <- data.frame(
    fit_a = as.character(pairs$fit_a),
    fit_b = as.character(pairs$fit_b),
    stringsAsFactors = FALSE
  )
  if (anyNA(output) || any(!nzchar(output$fit_a)) ||
        any(!nzchar(output$fit_b))) {
    .abort("Every requested fit pair must contain two non-empty fit IDs.")
  }
  if (any(output$fit_a == output$fit_b)) {
    .abort("`pairs` cannot compare a fit with itself.")
  }
  unknown <- setdiff(unique(c(output$fit_a, output$fit_b)), fit_ids)
  if (length(unknown)) {
    .abort(paste0(
      "Requested representation pairs include unknown or failed fits: ",
      paste(unknown, collapse = ", "), "."
    ))
  }
  keys <- vapply(seq_len(nrow(output)), function(i) {
    paste(sort(c(output$fit_a[[i]], output$fit_b[[i]])), collapse = "\r")
  }, character(1))
  if (anyDuplicated(keys)) {
    .abort("`pairs` must not repeat an unordered fit comparison.")
  }
  output
}

.empty_representation_failures <- function() {
  data.frame(
    stage = character(), fit_id = character(), fit_a = character(),
    fit_b = character(), scope = character(), error = character(),
    stringsAsFactors = FALSE
  )
}

.empty_representation_warnings <- function() {
  data.frame(
    stage = character(), fit_id = character(), fit_a = character(),
    fit_b = character(), scope = character(), warning_class = character(),
    warning = character(), stringsAsFactors = FALSE
  )
}

.summarise_repr_metrics <- function(fit_metrics, pairwise) {
  fit_summary <- if (!nrow(fit_metrics)) {
    data.frame(
      scope = character(), grid_id = integer(), xdim = integer(),
      ydim = integer(), n_fits = integer(), n_evaluable = integer(),
      median_mapping_coverage = numeric(),
      median_quantization_error = numeric(),
      median_topographic_error = numeric(),
      median_empty_unit_rate = numeric(), stringsAsFactors = FALSE
    )
  } else {
    groups <- split(
      fit_metrics,
      interaction(
        fit_metrics$scope, fit_metrics$grid_id,
        fit_metrics$xdim, fit_metrics$ydim,
        drop = TRUE, lex.order = TRUE
      )
    )
    do.call(rbind, lapply(groups, function(group) {
      finite_median <- function(value) {
        value <- value[is.finite(value)]
        if (length(value)) stats::median(value) else NA_real_
      }
      data.frame(
        scope = group$scope[[1L]],
        grid_id = group$grid_id[[1L]],
        xdim = group$xdim[[1L]],
        ydim = group$ydim[[1L]],
        n_fits = nrow(group),
        n_evaluable = sum(group$metric_status %in% c(
          "computed", "computed_with_partial_mapping"
        )),
        median_mapping_coverage = finite_median(group$mapping_coverage),
        median_quantization_error = finite_median(group$quantization_error),
        median_topographic_error = finite_median(group$topographic_error),
        median_empty_unit_rate = finite_median(group$empty_unit_rate),
        stringsAsFactors = FALSE
      )
    }))
  }
  rownames(fit_summary) <- NULL

  pairwise_summary <- if (!nrow(pairwise)) {
    data.frame(
      scope = character(), same_split = logical(), same_grid = logical(),
      n_pairs = integer(), n_rho_evaluable = integer(),
      median_rho = numeric(), rho_q025 = numeric(), rho_q975 = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    groups <- split(
      pairwise,
      interaction(
        pairwise$scope, pairwise$same_split, pairwise$same_grid,
        drop = TRUE, lex.order = TRUE
      )
    )
    do.call(rbind, lapply(groups, function(group) {
      rho <- group$distance_rank_correlation
      interval <- .quantile_safe(rho, c(0.025, 0.975))
      finite <- rho[is.finite(rho)]
      data.frame(
        scope = group$scope[[1L]],
        same_split = group$same_split[[1L]],
        same_grid = group$same_grid[[1L]],
        n_pairs = nrow(group),
        n_rho_evaluable = length(finite),
        median_rho = if (length(finite)) stats::median(finite) else NA_real_,
        rho_q025 = interval[[1L]],
        rho_q975 = interval[[2L]],
        stringsAsFactors = FALSE
      )
    }))
  }
  rownames(pairwise_summary) <- NULL
  list(fit_metrics = fit_summary, pairwise = pairwise_summary)
}

#' Audit reproducibility of continuous SOM representations
#'
#' Computes scope-specific representation diagnostics and compares the
#' topology induced by prespecified pairs of SOM fits. Pairwise evidence uses
#' shortest-hop distances on each SOM grid, so no node labels or map
#' orientations are forced into correspondence. All calculations are exact
#' under explicit fit-pair and sample-pair budgets.
#'
#' This experimental audit concerns continuous representation, not the
#' defensibility of a hard partition. It supplies no score, ranking, automatic
#' threshold or decision status and is not accepted by [assess_defensibility()].
#'
#' @param ensemble A fitted `som_ensemble` object. Retained `kohonen` model
#'   objects are not required.
#' @param scope Rows compared within each fit: `"analysis"` or `"assessment"`.
#' @param neighbourhood_size Optional positive integer `q` for exact local
#'   sample-neighbourhood Jaccard. All samples tied at the q-th shortest-hop
#'   distance are included. No scientific default is supplied.
#' @param pairs Optional prespecified data frame with `fit_a` and `fit_b`
#'   columns. The default compares all successful fits.
#' @param max_pairwise_comparisons Maximum number of fit pairs. The function
#'   stops before dense topology calculations when this budget is exceeded.
#' @param max_sample_pairs Maximum total number of unordered jointly mapped
#'   sample pairs across all requested fit comparisons.
#' @param fail_fast Whether to stop at the first metric-computation error.
#'
#' @return A `som_representation_audit` object with fit metrics, pairwise
#'   topology reproducibility, optional sample-neighbourhood records,
#'   descriptive summaries, computation budgets, provenance, warnings and
#'   failures. Sample pairs and fit pairs are not treated as independent
#'   inferential replicates.
#' @examples
#' data <- simulate_som_scenario("gradient", n = 45, p = 3, seed = 21)
#' ensemble <- fit_som_ensemble(
#'   data,
#'   som_spec(c(3, 2), seeds = 22:23, rlen = 10, k = 2),
#'   keep_models = FALSE
#' )
#' audit_som_representation(ensemble, neighbourhood_size = 5)
#' @export
audit_som_representation <- function(
  ensemble,
  scope = c("analysis", "assessment"),
  neighbourhood_size = NULL,
  pairs = NULL,
  max_pairwise_comparisons = 10000L,
  max_sample_pairs = 1000000L,
  fail_fast = FALSE
) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`ensemble` must come from `fit_som_ensemble()`.")
  }
  scope <- match.arg(scope)
  if (!is.null(neighbourhood_size)) {
    .assert_scalar_integer(neighbourhood_size, "neighbourhood_size", lower = 1)
    neighbourhood_size <- as.integer(neighbourhood_size)
  }
  .assert_scalar_integer(
    max_pairwise_comparisons, "max_pairwise_comparisons", lower = 1
  )
  .assert_scalar_integer(max_sample_pairs, "max_sample_pairs", lower = 1)
  .assert_flag(fail_fast, "fail_fast")

  n <- nrow(ensemble$data$metadata)
  sample_ids <- .validate_external_ids(
    ensemble$data$metadata$id, n, "ensemble$data$metadata$id"
  )
  resample_ids <- .validate_external_ids(
    ensemble$resamples$sample_ids,
    n,
    "ensemble$resamples$sample_ids"
  )
  if (!identical(sample_ids, resample_ids)) {
    .abort(paste0(
      "Ensemble data IDs do not match the ordered resampling sample IDs; ",
      "recompute the ensemble."
    ))
  }
  successful <- Filter(function(fit) isTRUE(fit$success), ensemble$fits)
  if (length(successful)) {
    invisible(lapply(
      successful,
      .validate_representation_fit,
      n = n,
      layers = ensemble$data$layers,
      splits = ensemble$resamples$splits
    ))
  }
  fit_ids <- vapply(successful, `[[`, character(1), "id")
  if (anyDuplicated(fit_ids)) {
    .abort("Successful ensemble fits must have unique IDs.")
  }
  pair_design <- .normalise_repr_pairs(pairs, fit_ids)
  if (nrow(pair_design) > max_pairwise_comparisons) {
    .abort(paste0(
      "The representation audit requests ", nrow(pair_design),
      " fit pairs, exceeding `max_pairwise_comparisons = ",
      max_pairwise_comparisons, "`. Prespecify a smaller `pairs` table or ",
      "increase the budget deliberately."
    ))
  }
  fit_lookup <- stats::setNames(successful, fit_ids)
  planned_sample_pairs <- if (nrow(pair_design)) {
    sum(vapply(seq_len(nrow(pair_design)), function(i) {
      first <- fit_lookup[[pair_design$fit_a[[i]]]]
      second <- fit_lookup[[pair_design$fit_b[[i]]]]
      shared <- intersect(
        .representation_scope_rows(first, scope),
        .representation_scope_rows(second, scope)
      )
      jointly_mapped <- shared[
        .representation_mapped(first, shared) &
          .representation_mapped(second, shared)
      ]
      choose(length(jointly_mapped), 2)
    }, numeric(1)))
  } else {
    0
  }
  if (!is.finite(planned_sample_pairs) ||
        planned_sample_pairs > max_sample_pairs) {
    .abort(paste0(
      "The representation audit requests ",
      format(planned_sample_pairs, scientific = FALSE),
      " jointly mapped sample-pair comparisons, exceeding ",
      "`max_sample_pairs = ", max_sample_pairs,
      "`. Prespecify fewer fit pairs or increase the budget deliberately."
    ))
  }

  failures <- .empty_representation_failures()
  if (nrow(ensemble$failures)) {
    failures <- data.frame(
      stage = "ensemble_fit",
      fit_id = ensemble$failures$id,
      fit_a = NA_character_, fit_b = NA_character_, scope = scope,
      error = ensemble$failures$error,
      stringsAsFactors = FALSE
    )
  }
  warnings <- .empty_representation_warnings()
  fit_metrics <- lapply(successful, function(fit) {
    rows <- .representation_scope_rows(fit, scope)
    mapped <- .representation_mapped(fit, rows)
    mapped_rows <- rows[mapped]
    n_requested <- length(rows)
    n_mapped <- length(mapped_rows)
    metric_status <- if (!n_requested) {
      "no_scope_rows"
    } else if (!n_mapped) {
      "no_mapped_rows"
    } else if (n_mapped < n_requested) {
      "computed_with_partial_mapping"
    } else {
      "computed"
    }
    quantization_error <- if (n_mapped) {
      mean(fit$distances[mapped_rows])
    } else {
      NA_real_
    }
    topographic_error <- NA_real_
    if (n_mapped) {
      captured <- .capture_warnings({
        if (scope == "analysis" &&
              is.finite(fit$training_topographic_error %||% NA_real_)) {
          fit$training_topographic_error
        } else {
          .representation_te(ensemble, fit, mapped_rows)
        }
      })
      if (inherits(captured$value, "error")) {
        if (fail_fast) stop(captured$value)
        metric_status <- "metric_error"
        failures <<- rbind(
          failures,
          data.frame(
            stage = "fit_metric", fit_id = fit$id,
            fit_a = NA_character_, fit_b = NA_character_, scope = scope,
            error = conditionMessage(captured$value),
            stringsAsFactors = FALSE
          )
        )
      } else {
        topographic_error <- captured$value
      }
      if (nrow(captured$warnings)) {
        warnings <<- rbind(
          warnings,
          data.frame(
            stage = "fit_metric", fit_id = fit$id,
            fit_a = NA_character_, fit_b = NA_character_, scope = scope,
            warning_class = captured$warnings$warning_class,
            warning = captured$warnings$warning,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    units <- nrow(fit$codes[[1L]])
    empty_unit_rate <- if (n_mapped) {
      1 - length(unique(fit$bmu[mapped_rows])) / units
    } else {
      NA_real_
    }
    analysis_rows <- fit$analysis[
      .representation_mapped(fit, fit$analysis)
    ]
    relative_unoccupied <- if (scope == "assessment" && n_mapped &&
                                 length(analysis_rows)) {
      .unoccupied_rate(
        fit$bmu[mapped_rows],
        unique(fit$bmu[analysis_rows])
      )
    } else {
      NA_real_
    }
    data.frame(
      fit_id = fit$id,
      split_id = fit$split_id,
      grid_id = fit$grid_id,
      xdim = fit$xdim,
      ydim = fit$ydim,
      seed = fit$seed,
      scope = scope,
      n_requested = n_requested,
      n_mapped = n_mapped,
      mapping_coverage = if (n_requested) n_mapped / n_requested else NA_real_,
      quantization_error = quantization_error,
      topographic_error = topographic_error,
      empty_unit_rate = empty_unit_rate,
      unoccupied_unit_rate_relative_to_analysis = relative_unoccupied,
      metric_status = metric_status,
      stringsAsFactors = FALSE
    )
  })
  fit_metrics <- if (length(fit_metrics)) {
    do.call(rbind, fit_metrics)
  } else {
    data.frame(
      fit_id = character(), split_id = character(), grid_id = integer(),
      xdim = integer(), ydim = integer(), seed = integer(), scope = character(),
      n_requested = integer(), n_mapped = integer(),
      mapping_coverage = numeric(), quantization_error = numeric(),
      topographic_error = numeric(), empty_unit_rate = numeric(),
      unoccupied_unit_rate_relative_to_analysis = numeric(),
      metric_status = character(), stringsAsFactors = FALSE
    )
  }

  graph_cache <- new.env(parent = emptyenv())
  graph_for <- function(fit) {
    key <- .som_grid_key(fit$grid)
    if (!exists(key, envir = graph_cache, inherits = FALSE)) {
      assign(key, .som_grid_hop_distances(fit$grid), envir = graph_cache)
    }
    get(key, envir = graph_cache, inherits = FALSE)
  }
  neighbourhood_records <- list()
  pairwise_records <- lapply(seq_len(nrow(pair_design)), function(i) {
    first <- fit_lookup[[pair_design$fit_a[[i]]]]
    second <- fit_lookup[[pair_design$fit_b[[i]]]]
    shared <- intersect(
      .representation_scope_rows(first, scope),
      .representation_scope_rows(second, scope)
    )
    jointly_mapped <- shared[
      .representation_mapped(first, shared) &
        .representation_mapped(second, shared)
    ]
    n_common_design <- length(shared)
    n_common_mapped <- length(jointly_mapped)
    n_sample_pairs <- choose(n_common_mapped, 2)
    rho <- NA_real_
    rho_status <- if (!n_common_design) {
      "no_common_design_rows"
    } else if (n_common_mapped < 3L) {
      "too_few_jointly_mapped"
    } else {
      "computed"
    }
    jaccard_status <- if (is.null(neighbourhood_size)) {
      "not_requested"
    } else if (n_common_mapped <= neighbourhood_size) {
      "insufficient_common_samples"
    } else {
      "computed"
    }
    median_jaccard <- jaccard_q025 <- jaccard_q975 <-
      median_neighbours_a <- median_neighbours_b <- NA_real_

    needs_graph <- n_common_mapped >= 3L ||
      (!is.null(neighbourhood_size) &&
         n_common_mapped > neighbourhood_size)
    topology <- NULL
    if (needs_graph) {
      captured <- .capture_warnings({
        list(graph_a = graph_for(first), graph_b = graph_for(second))
      })
      if (inherits(captured$value, "error")) {
        if (fail_fast) stop(captured$value)
        failures <<- rbind(
          failures,
          data.frame(
            stage = "pairwise_topology", fit_id = NA_character_,
            fit_a = first$id, fit_b = second$id, scope = scope,
            error = conditionMessage(captured$value),
            stringsAsFactors = FALSE
          )
        )
        if (n_common_mapped >= 3L) rho_status <- "metric_error"
        if (!is.null(neighbourhood_size) &&
              n_common_mapped > neighbourhood_size) {
          jaccard_status <- "metric_error"
        }
      } else {
        topology <- captured$value
      }
      if (nrow(captured$warnings)) {
        warnings <<- rbind(
          warnings,
          data.frame(
            stage = "pairwise_topology", fit_id = NA_character_,
            fit_a = first$id, fit_b = second$id, scope = scope,
            warning_class = captured$warnings$warning_class,
            warning = captured$warnings$warning,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    if (!is.null(topology)) {
      graph_a <- topology$graph_a
      graph_b <- topology$graph_b
      bmu_a <- as.integer(first$bmu[jointly_mapped])
      bmu_b <- as.integer(second$bmu[jointly_mapped])
      distances_a <- graph_a[bmu_a, bmu_a, drop = FALSE]
      distances_b <- graph_b[bmu_b, bmu_b, drop = FALSE]
      if (n_common_mapped >= 3L) {
        lower <- lower.tri(distances_a)
        vector_a <- distances_a[lower]
        vector_b <- distances_b[lower]
        if (length(unique(vector_a)) < 2L ||
              length(unique(vector_b)) < 2L) {
          rho_status <- "constant_topological_distances"
        } else {
          rho <- stats::cor(vector_a, vector_b, method = "spearman")
        }
      }

      if (!is.null(neighbourhood_size) &&
            n_common_mapped > neighbourhood_size) {
        records <- lapply(seq_len(n_common_mapped), function(anchor) {
          candidates <- setdiff(seq_len(n_common_mapped), anchor)
          cutoff_a <- sort(distances_a[anchor, candidates])[[
            neighbourhood_size
          ]]
          cutoff_b <- sort(distances_b[anchor, candidates])[[
            neighbourhood_size
          ]]
          neighbours_a <- candidates[
            distances_a[anchor, candidates] <= cutoff_a
          ]
          neighbours_b <- candidates[
            distances_b[anchor, candidates] <= cutoff_b
          ]
          intersection_n <- length(intersect(neighbours_a, neighbours_b))
          union_n <- length(union(neighbours_a, neighbours_b))
          data.frame(
            fit_a = first$id,
            fit_b = second$id,
            scope = scope,
            sample_id = sample_ids[jointly_mapped[[anchor]]],
            neighbourhood_size = neighbourhood_size,
            n_neighbours_a = length(neighbours_a),
            n_neighbours_b = length(neighbours_b),
            intersection_n = intersection_n,
            union_n = union_n,
            jaccard = if (union_n) intersection_n / union_n else NA_real_,
            stringsAsFactors = FALSE
          )
        })
        records <- do.call(rbind, records)
        neighbourhood_records[[length(neighbourhood_records) + 1L]] <<- records
        interval <- .quantile_safe(records$jaccard, c(0.025, 0.975))
        median_jaccard <- stats::median(records$jaccard, na.rm = TRUE)
        jaccard_q025 <- interval[[1L]]
        jaccard_q975 <- interval[[2L]]
        median_neighbours_a <- stats::median(records$n_neighbours_a)
        median_neighbours_b <- stats::median(records$n_neighbours_b)
      }
    }

    data.frame(
      fit_a = first$id,
      fit_b = second$id,
      split_a = first$split_id,
      split_b = second$split_id,
      grid_a = first$grid_id,
      grid_b = second$grid_id,
      same_split = identical(first$split_id, second$split_id),
      same_grid = identical(first$grid_id, second$grid_id),
      scope = scope,
      n_common_design = n_common_design,
      n_common_mapped = n_common_mapped,
      joint_mapping_coverage = if (n_common_design) {
        n_common_mapped / n_common_design
      } else {
        NA_real_
      },
      n_sample_pairs = n_sample_pairs,
      distance_rank_correlation = rho,
      correlation_status = rho_status,
      neighbourhood_size = neighbourhood_size %||% NA_integer_,
      median_neighbourhood_jaccard = median_jaccard,
      neighbourhood_jaccard_q025 = jaccard_q025,
      neighbourhood_jaccard_q975 = jaccard_q975,
      median_neighbourhood_size_a = median_neighbours_a,
      median_neighbourhood_size_b = median_neighbours_b,
      neighbourhood_status = jaccard_status,
      stringsAsFactors = FALSE
    )
  })
  pairwise <- if (length(pairwise_records)) {
    do.call(rbind, pairwise_records)
  } else {
    data.frame(
      fit_a = character(), fit_b = character(), split_a = character(),
      split_b = character(), grid_a = integer(), grid_b = integer(),
      same_split = logical(), same_grid = logical(), scope = character(),
      n_common_design = integer(), n_common_mapped = integer(),
      joint_mapping_coverage = numeric(), n_sample_pairs = numeric(),
      distance_rank_correlation = numeric(), correlation_status = character(),
      neighbourhood_size = integer(),
      median_neighbourhood_jaccard = numeric(),
      neighbourhood_jaccard_q025 = numeric(),
      neighbourhood_jaccard_q975 = numeric(),
      median_neighbourhood_size_a = numeric(),
      median_neighbourhood_size_b = numeric(),
      neighbourhood_status = character(), stringsAsFactors = FALSE
    )
  }
  neighbourhood_records <- if (length(neighbourhood_records)) {
    do.call(rbind, neighbourhood_records)
  } else {
    data.frame(
      fit_a = character(), fit_b = character(), scope = character(),
      sample_id = character(), neighbourhood_size = integer(),
      n_neighbours_a = integer(), n_neighbours_b = integer(),
      intersection_n = integer(), union_n = integer(), jaccard = numeric(),
      stringsAsFactors = FALSE
    )
  }
  summary <- .summarise_repr_metrics(fit_metrics, pairwise)

  .new_som_object(
    list(
      fit_metrics = fit_metrics,
      pairwise = pairwise,
      neighbourhood_records = neighbourhood_records,
      summary = summary,
      failures = failures,
      warnings = warnings,
      comparison_budget = list(
        scope = scope,
        successful_fits = length(successful),
        planned_fit_pairs = nrow(pair_design),
        planned_sample_pairs = planned_sample_pairs,
        max_pairwise_comparisons = as.integer(max_pairwise_comparisons),
        max_sample_pairs = as.integer(max_sample_pairs),
        exact = TRUE
      ),
      provenance = list(
        sample_ids = sample_ids,
        fit_ids = fit_ids,
        graph_distance = "shortest_hop",
        neighbourhood_tie_policy = "include_qth_distance_ties",
        independent_unit_warning = paste0(
          "Fit pairs and sample pairs are dependent descriptive units, not ",
          "independent inferential replicates."
        )
      ),
      ensemble = ensemble
    ),
    "som_representation_audit"
  )
}

#' @export
print.som_representation_audit <- function(x, ...) {
  computed_fits <- sum(x$fit_metrics$metric_status %in% c(
    "computed", "computed_with_partial_mapping"
  ))
  computed_pairs <- sum(x$pairwise$correlation_status == "computed")
  cat("<som_representation_audit> (experimental)\n")
  cat("  scope             :", x$comparison_budget$scope, "\n")
  cat("  fit metrics       :", computed_fits, "of", nrow(x$fit_metrics), "\n")
  cat("  topology pairs    :", computed_pairs, "of", nrow(x$pairwise), "\n")
  cat("  sample-pair budget:",
      format(x$comparison_budget$planned_sample_pairs, scientific = FALSE),
      "of", x$comparison_budget$max_sample_pairs, "\n")
  cat("  interpretation    : representation evidence, not a class decision\n")
  invisible(x)
}
