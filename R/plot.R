.require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .abort("Install the suggested package `ggplot2` to create this plot.")
  }
}

.select_successful_fit <- function(ensemble, fit_id) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`x` must come from `fit_som_ensemble()`.")
  }
  successful <- Filter(function(fit) isTRUE(fit$success), ensemble$fits)
  if (!length(successful)) .abort("No successful SOM fit is available.")
  ids <- vapply(successful, `[[`, character(1), "id")
  if (is.null(fit_id)) {
    if (length(successful) > 1L) {
      .abort(paste0(
        "Select `fit_id` explicitly; this ensemble has ", length(ids),
        " successful fits. Examples: ",
        paste(utils::head(ids, 3L), collapse = ", "), "."
      ))
    }
    return(successful[[1L]])
  }
  if (!is.character(fit_id) || length(fit_id) != 1L || is.na(fit_id) ||
        !fit_id %in% ids) {
    .abort("`fit_id` must identify one successful fit in the ensemble.")
  }
  successful[[match(fit_id, ids)]]
}

.som_grid_geometry <- function(grid, panels) {
  points <- as.data.frame(grid$pts)
  points$unit <- seq_len(nrow(points))
  if (grid$topo == "rectangular") {
    output <- do.call(rbind, lapply(panels, function(panel) {
      transform(points, panel = panel)
    }))
    output$geometry <- "tile"
    return(output)
  }

  centre_distance <- as.matrix(stats::dist(grid$pts))
  radius <- min(centre_distance[centre_distance > 0]) / sqrt(3)
  angles <- pi / 6 + seq(0, 2 * pi, length.out = 7L)
  polygons <- lapply(panels, function(panel) {
    do.call(rbind, lapply(seq_len(nrow(points)), function(unit) {
      data.frame(
        x = points$x[[unit]] + radius * cos(angles),
        y = points$y[[unit]] + radius * sin(angles),
        unit = unit,
        vertex = seq_along(angles),
        panel = panel,
        stringsAsFactors = FALSE
      )
    }))
  })
  output <- do.call(rbind, polygons)
  output$geometry <- "polygon"
  output
}

#' Plot an interpretable plane from one SOM ensemble member
#'
#' This function deliberately plots a named ensemble member instead of
#' silently treating one fitted map as representative of the ensemble.
#' Component values are on the processed scale used for training.
#'
#' @param x A `som_ensemble` object.
#' @param type One of `"component"`, `"occupancy"` or
#'   `"neighbour_distance"`.
#' @param fit_id Identifier of one successful ensemble fit. It may be omitted
#'   only when the ensemble contains exactly one successful fit.
#' @param layer Layer name used for a component plane. It may be omitted for a
#'   single-layer analysis.
#' @param variables Optional component names. All variables in `layer` are
#'   shown by default.
#'
#' @return An editable `ggplot` object.
#' @export
plot_som_plane <- function(
  x,
  type = c("component", "occupancy", "neighbour_distance"),
  fit_id = NULL,
  layer = NULL,
  variables = NULL
) {
  .require_ggplot2()
  y <- panel <- unit <- value <- NULL
  type <- match.arg(type)
  fit <- .select_successful_fit(x, fit_id)

  if (type == "component") {
    layer_names <- names(fit$codes)
    if (is.null(layer)) {
      if (length(layer_names) > 1L) {
        .abort("Select `layer` explicitly for a multi-layer SOM.")
      }
      layer <- layer_names[[1L]]
    }
    if (!is.character(layer) || length(layer) != 1L || !layer %in% layer_names) {
      .abort("`layer` must identify one layer in the selected SOM fit.")
    }
    codebook <- fit$codes[[layer]]
    variables <- variables %||% colnames(codebook)
    if (!is.character(variables) || !length(variables) || anyNA(variables) ||
          !all(variables %in% colnames(codebook))) {
      .abort("`variables` must name columns in the selected codebook layer.")
    }
    values <- do.call(rbind, lapply(variables, function(variable) {
      data.frame(
        unit = seq_len(nrow(codebook)),
        panel = variable,
        value = codebook[, variable],
        stringsAsFactors = FALSE
      )
    }))
    fill_label <- "Codebook value\n(processed scale)"
  } else if (type == "occupancy") {
    values <- data.frame(
      unit = seq_len(nrow(fit$grid$pts)),
      panel = "Training-sample occupancy",
      value = tabulate(fit$bmu[fit$analysis], nbins = nrow(fit$grid$pts))
    )
    fill_label <- "Training samples"
  } else {
    codebook <- .weighted_codes(fit)
    unit_distance <- kohonen::unit.distances(fit$grid)
    nearest <- min(unit_distance[unit_distance > 0]) * (1 + 1e-8)
    value <- vapply(seq_len(nrow(codebook)), function(unit) {
      neighbours <- which(
        unit_distance[unit, ] > 0 & unit_distance[unit, ] <= nearest
      )
      if (!length(neighbours)) return(NA_real_)
      delta <- sweep(codebook[neighbours, , drop = FALSE], 2L, codebook[unit, ], "-")
      mean(sqrt(rowSums(delta^2)))
    }, numeric(1))
    values <- data.frame(
      unit = seq_len(nrow(codebook)),
      panel = "Mean adjacent codebook distance",
      value = value
    )
    fill_label <- "Codebook distance"
  }

  geometry <- .som_grid_geometry(fit$grid, unique(values$panel))
  data <- merge(geometry, values, by = c("unit", "panel"), sort = FALSE)
  mapping <- ggplot2::aes(x = x, y = y, fill = value)
  figure <- ggplot2::ggplot(data, mapping)
  if (fit$grid$topo == "rectangular") {
    figure <- figure + ggplot2::geom_tile(colour = "white", linewidth = 0.25)
  } else {
    figure <- figure + ggplot2::geom_polygon(
      ggplot2::aes(group = interaction(panel, unit)),
      colour = "white", linewidth = 0.25
    )
  }
  figure +
    ggplot2::scale_fill_viridis_c(option = "C", na.value = "grey90") +
    ggplot2::facet_wrap(~panel) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = NULL, y = NULL, fill = fill_label,
      subtitle = paste("Ensemble member:", fit$id)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "right",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

#' Plot SOM audit trade-offs
#'
#' @param x A `som_audit` object.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot.som_audit <- function(x, ...) {
  .require_ggplot2()
  data <- x$fit_metrics
  data$grid <- paste0(data$xdim, "x", data$ydim)
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = quantization_error,
      y = topographic_error,
      colour = grid,
      size = empty_unit_rate
    )
  ) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::labs(
      x = "Quantization error",
      y = "Topographic error",
      colour = "Grid",
      size = "Empty-unit rate"
    ) +
    ggplot2::theme_minimal()
}

#' Plot partition reproducibility across candidate class counts
#'
#' @param x A `som_partitions` object.
#' @param ... Unused.
#'
#' @return A faceted `ggplot` object with ensemble agreement intervals.
#' @export
plot.som_partitions <- function(x, ...) {
  .require_ggplot2()
  median <- lower <- upper <- NULL
  if (!nrow(x$stability)) {
    .abort("Partition stability requires at least two successful SOM fits.")
  }
  data <- rbind(
    data.frame(
      k = x$stability$k,
      metric = "ARI",
      median = x$stability$median_ari,
      lower = x$stability$ari_q025,
      upper = x$stability$ari_q975
    ),
    data.frame(
      k = x$stability$k,
      metric = "AMI",
      median = x$stability$median_ami,
      lower = x$stability$ami_q025,
      upper = x$stability$ami_q975
    )
  )
  data$metric <- factor(data$metric, levels = c("ARI", "AMI"))
  ggplot2::ggplot(data, ggplot2::aes(x = k, y = median)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      fill = "#56B4E9", alpha = 0.25
    ) +
    ggplot2::geom_line(colour = "#0072B2", linewidth = 0.7) +
    ggplot2::geom_point(colour = "#0072B2", size = 2) +
    ggplot2::facet_wrap(~metric) +
    ggplot2::scale_x_continuous(breaks = sort(unique(data$k))) +
    ggplot2::coord_cartesian(ylim = c(-1, 1)) +
    ggplot2::labs(
      x = "Candidate number of clusters",
      y = "Pairwise partition agreement",
      subtitle = "Median and 2.5th-97.5th percentile across ensemble pairs"
    ) +
    ggplot2::theme_minimal()
}

#' Plot sample-level consensus evidence
#'
#' @param x A `som_consensus` object.
#' @param type Either `"support"` or `"heatmap"`. A heatmap requires a
#'   co-assignment consensus; aligned voting does not create a dense
#'   co-assignment matrix.
#' @param samples Optional sample indices for a heatmap.
#' @param max_samples Maximum heatmap size allowed without explicit `samples`.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot.som_consensus <- function(
  x, type = c("support", "heatmap"), samples = NULL,
  max_samples = 500L, ...
) {
  .require_ggplot2()
  type <- match.arg(type)
  if (type == "support") {
    data <- data.frame(
      sample = seq_along(x$consensus_labels),
      cluster = factor(x$consensus_labels),
      support = x$membership_support,
      entropy = x$assignment_entropy
    )
    return(
      ggplot2::ggplot(
        data,
        ggplot2::aes(x = support, y = entropy, colour = cluster)
      ) +
        ggplot2::geom_point(alpha = 0.75) +
        ggplot2::labs(
          x = "Membership support",
          y = "Normalized assignment entropy",
          colour = "Consensus cluster"
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (is.null(x$coassignment)) {
    .abort(paste0(
      "This consensus used `aligned_vote` and has no dense co-assignment ",
      "matrix. Draw the support plot or rerun an explicitly bounded ",
      "co-assignment analysis."
    ))
  }
  n <- nrow(x$coassignment)
  if (is.null(samples)) {
    if (n > max_samples) {
      .abort(paste0(
        "The consensus matrix has ", n, " samples; select `samples` ",
        "explicitly before drawing a heatmap."
      ))
    }
    samples <- seq_len(n)
  }
  samples <- .validate_index(samples, n, "samples")
  order_index <- samples[order(x$consensus_labels[samples], samples)]
  matrix <- x$coassignment[order_index, order_index, drop = FALSE]
  data <- data.frame(
    row = rep(seq_along(order_index), times = length(order_index)),
    column = rep(seq_along(order_index), each = length(order_index)),
    coassignment = as.vector(matrix)
  )
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = column, y = row, fill = coassignment)
  ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1)) +
    ggplot2::coord_equal() +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(
      x = "Ordered sample",
      y = "Ordered sample",
      fill = "Co-assignment"
    ) +
    ggplot2::theme_minimal()
}

#' Plot sample-level sensitivity across prespecified scenarios
#'
#' Each point represents one sample at one candidate `k`. The horizontal axis
#' conditions cluster-member sets on jointly evaluable shared samples and
#' therefore isolates repartitioning. The vertical axis also includes eligible
#' members unique to either scenario and therefore combines data coverage with
#' repartitioning. Values are medians across evaluable contrasts. Membership
#' sets are compared directly, so the plot is invariant to arbitrary
#' cluster-label permutations. It describes sensitivity to the scenarios that
#' were supplied, not a probability that a cluster assignment is correct.
#'
#' @param x A `som_sensitivity` object.
#' @param k Optional candidate cluster counts to display.
#' @param ... Unused.
#'
#' @return An editable `ggplot` object.
#' @export
plot.som_sensitivity <- function(x, k = NULL, ...) {
  .require_ggplot2()
  shared_jaccard <- all_jaccard <- contrast_coverage <- n_contrasts <- NULL
  data <- x$sample_summary
  if (!is.null(k)) {
    if (!is.numeric(k) || !length(k) || anyNA(k) || any(k < 2) ||
          any(k != as.integer(k))) {
      .abort("`k` must contain candidate cluster counts of at least 2.")
    }
    data <- data[data$k %in% as.integer(k), , drop = FALSE]
  }
  if (!nrow(data)) {
    .abort("No evaluable sample-level scenario contrasts are available.")
  }
  data$shared_jaccard <- data$median_membership_jaccard_shared
  data$all_jaccard <- data$median_membership_jaccard_all

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = shared_jaccard,
      y = all_jaccard,
      colour = contrast_coverage,
      size = n_contrasts
    )
  ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, colour = "grey65", linewidth = 0.4
    ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::facet_wrap(~k, labeller = ggplot2::label_both) +
    ggplot2::scale_colour_viridis_c(limits = c(0, 1), option = "C") +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Median Jaccard within shared sample universe",
      y = "Median Jaccard including scenario-specific members",
      colour = "Contrast\ncoverage",
      size = "Evaluable\ncontrasts",
      subtitle = "Across prespecified analysis scenarios"
    ) +
    ggplot2::theme_minimal()
}

#' Plot cross-model agreement without ranking methods
#'
#' @param x A `som_cross_comparison` object.
#' @param ... Unused.
#'
#' @return A faceted `ggplot` object showing ARI and AMI distributions.
#' @export
plot.som_cross_comparison <- function(x, ...) {
  .require_ggplot2()
  data <- rbind(
    data.frame(
      method = x$comparisons$method, k = factor(x$comparisons$k),
      metric = "ARI", agreement = x$comparisons$ari
    ),
    data.frame(
      method = x$comparisons$method, k = factor(x$comparisons$k),
      metric = "AMI", agreement = x$comparisons$ami
    )
  )
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = k, y = agreement, colour = method)
  ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = interaction(k, method)),
      position = ggplot2::position_dodge(width = 0.75),
      outlier.alpha = 0.35
    ) +
    ggplot2::facet_wrap(~metric) +
    ggplot2::labs(
      x = "Candidate number of clusters",
      y = "Partition agreement",
      colour = "Reference method"
    ) +
    ggplot2::theme_minimal()
}

#' Plot mapping shift in held-out domains
#'
#' @param x A `som_transfer_audit` object.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot.som_transfer_audit <- function(x, ...) {
  .require_ggplot2()
  ggplot2::ggplot(
    x$metrics,
    ggplot2::aes(
      x = median_analysis_distance,
      y = median_assessment_distance,
      colour = factor(grid_id),
      size = unoccupied_unit_rate
    )
  ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, linetype = 2,
      colour = "grey45"
    ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::labs(
      x = "Median analysis-set mapping distance",
      y = "Median assessment-set mapping distance",
      colour = "Grid ID",
      size = "Mapped to units\nunoccupied in training"
    ) +
    ggplot2::theme_minimal()
}

#' Plot new-data mapping shift across an ensemble
#'
#' @param x A `som_newdata_mapping` object.
#' @param ... Unused.
#'
#' @return An editable `ggplot` object.
#' @export
plot.som_newdata_mapping <- function(x, ...) {
  .require_ggplot2()
  median_training_distance <- median_new_distance <- grid_id <-
    unoccupied_unit_rate <- NULL
  if (!nrow(x$summary)) .abort("No successful new-data mappings are available.")
  ggplot2::ggplot(
    x$summary,
    ggplot2::aes(
      x = median_training_distance,
      y = median_new_distance,
      colour = factor(grid_id),
      size = unoccupied_unit_rate
    )
  ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, linetype = 2,
      colour = "grey45"
    ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::labs(
      x = "Median training-set mapping distance",
      y = "Median new-data mapping distance",
      colour = "Grid ID",
      size = "Mapped to units\nunoccupied in training"
    ) +
    ggplot2::theme_minimal()
}
