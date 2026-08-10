.model_distance_matrix <- function(fit) {
  layers <- fit$processed_all
  codes <- fit$codes
  weights <- fit$user_weights * fit$distance_weights
  weights <- weights / sum(weights)
  n <- nrow(layers[[1L]])
  units <- nrow(codes[[1L]])
  total <- matrix(0, nrow = n, ncol = units)
  available_weight <- matrix(0, nrow = n, ncol = units)

  for (i in seq_along(layers)) {
    x <- layers[[i]]
    code <- codes[[i]]
    p <- ncol(x)
    observed_n <- rowSums(!is.na(x))
    layer_d <- matrix(NA_real_, nrow = n, ncol = units)
    valid <- observed_n > 0
    for (u in seq_len(units)) {
      delta <- sweep(x, 2L, code[u, ], "-")
      layer_d[valid, u] <- rowSums(delta[valid, , drop = FALSE]^2, na.rm = TRUE) *
        p / observed_n[valid]
    }
    present <- is.finite(layer_d)
    total[present] <- total[present] + weights[[i]] * layer_d[present]
    available_weight[present] <- available_weight[present] + weights[[i]]
  }
  total[available_weight > 0] <- total[available_weight > 0] /
    available_weight[available_weight > 0]
  total[available_weight == 0] <- NA_real_
  total
}

.topographic_error <- function(fit, rows) {
  if (!isTRUE(fit$success)) {
    return(NA_real_)
  }
  distance_matrix <- .model_distance_matrix(fit)[rows, , drop = FALSE]
  if (ncol(distance_matrix) < 2L) {
    return(NA_real_)
  }
  unit_distance <- kohonen::unit.distances(fit$grid)
  positive <- unit_distance[unit_distance > 0]
  adjacency_limit <- min(positive) * (1 + 1e-8)
  errors <- apply(distance_matrix, 1L, function(d) {
    if (sum(is.finite(d)) < 2L) {
      return(NA_real_)
    }
    nearest <- order(d)[1:2]
    as.numeric(unit_distance[nearest[1L], nearest[2L]] > adjacency_limit)
  })
  mean(errors, na.rm = TRUE)
}

#' Audit representation quality across a SOM ensemble
#'
#' @param ensemble A fitted `som_ensemble`.
#' @return A `som_audit` object containing fit-level metrics and summaries.
#' @export
audit_som <- function(ensemble) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`ensemble` must come from `fit_som_ensemble()`.")
  }
  successful <- Filter(function(x) isTRUE(x$success), ensemble$fits)
  if (!length(successful)) {
    fit_metrics <- data.frame(
      id = character(), split_id = character(), grid_id = integer(),
      xdim = integer(), ydim = integer(), seed = integer(),
      quantization_error = numeric(), topographic_error = numeric(),
      empty_unit_rate = numeric(), stringsAsFactors = FALSE
    )
    grid_summary <- data.frame(
      xdim = integer(), ydim = integer(), n_fits = integer(),
      median_quantization_error = numeric(),
      median_topographic_error = numeric(),
      median_empty_unit_rate = numeric(), stringsAsFactors = FALSE
    )
    return(structure(
      list(
        fit_metrics = fit_metrics,
        grid_summary = grid_summary,
        success_rate = 0,
        failures = ensemble$failures,
        ensemble = ensemble
      ),
      class = "som_audit"
    ))
  }

  fit_metrics <- do.call(rbind, lapply(successful, function(fit) {
    data.frame(
      id = fit$id,
      split_id = fit$split_id,
      grid_id = fit$grid_id,
      xdim = fit$xdim,
      ydim = fit$ydim,
      seed = fit$seed,
      quantization_error = fit$training_quantization_error,
      topographic_error = .topographic_error(fit, fit$analysis),
      empty_unit_rate = fit$empty_unit_rate,
      stringsAsFactors = FALSE
    )
  }))

  key <- interaction(fit_metrics$xdim, fit_metrics$ydim, drop = TRUE)
  summary_rows <- lapply(split(fit_metrics, key), function(z) {
    data.frame(
      xdim = z$xdim[[1L]],
      ydim = z$ydim[[1L]],
      n_fits = nrow(z),
      median_quantization_error = stats::median(
        z$quantization_error,
        na.rm = TRUE
      ),
      median_topographic_error = stats::median(
        z$topographic_error,
        na.rm = TRUE
      ),
      median_empty_unit_rate = stats::median(
        z$empty_unit_rate,
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  })

  structure(
    list(
      fit_metrics = fit_metrics,
      grid_summary = do.call(rbind, summary_rows),
      success_rate = nrow(fit_metrics) / ensemble$expected_models,
      failures = ensemble$failures,
      ensemble = ensemble
    ),
    class = "som_audit"
  )
}

#' @export
print.som_audit <- function(x, ...) {
  cat("<som_audit>\n")
  cat("  successful fits:", nrow(x$fit_metrics), "\n")
  cat("  success rate   :", sprintf("%.1f%%", 100 * x$success_rate), "\n")
  cat("  grids audited  :", nrow(x$grid_summary), "\n")
  invisible(x)
}

.adjusted_rand <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  n <- length(x)
  if (n < 2L) {
    return(NA_real_)
  }
  tab <- table(x, y)
  choose2 <- function(z) z * (z - 1) / 2
  index <- sum(choose2(tab))
  row_pairs <- sum(choose2(rowSums(tab)))
  col_pairs <- sum(choose2(colSums(tab)))
  total_pairs <- choose2(n)
  expected <- row_pairs * col_pairs / total_pairs
  maximum <- 0.5 * (row_pairs + col_pairs)
  denominator <- maximum - expected
  if (abs(denominator) < .Machine$double.eps) {
    return(if (all(outer(x, x, "==") == outer(y, y, "=="))) 1 else 0)
  }
  (index - expected) / denominator
}

.label_entropy <- function(x) {
  probability <- as.numeric(table(x)) / length(x)
  -sum(probability * log(probability))
}

.expected_mutual_information <- function(tab) {
  n <- sum(tab)
  row_n <- rowSums(tab)
  col_n <- colSums(tab)
  expected <- 0
  for (a in row_n) {
    for (b in col_n) {
      lower <- max(1, a + b - n)
      upper <- min(a, b)
      if (lower > upper) next
      overlap <- seq.int(lower, upper)
      log_probability <- lchoose(a, overlap) + lchoose(n - a, b - overlap) -
        lchoose(n, b)
      expected <- expected + sum(
        (overlap / n) * log(n * overlap / (a * b)) * exp(log_probability)
      )
    }
  }
  expected
}

.adjusted_mutual_info <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  tab <- table(x, y)
  n <- sum(tab)
  nonzero <- tab > 0
  row_n <- rowSums(tab)
  col_n <- colSums(tab)
  mutual_information <- sum(
    (tab[nonzero] / n) * log(
      n * tab[nonzero] /
        outer(row_n, col_n)[nonzero]
    )
  )
  expected <- .expected_mutual_information(tab)
  normalizer <- mean(c(.label_entropy(x), .label_entropy(y)))
  denominator <- normalizer - expected
  if (abs(denominator) < sqrt(.Machine$double.eps)) {
    return(if (all(outer(x, x, "==") == outer(y, y, "=="))) 1 else 0)
  }
  value <- (mutual_information - expected) / denominator
  max(-1, min(1, value))
}

.weighted_codes <- function(fit) {
  weights <- fit$user_weights * fit$distance_weights
  weights <- weights / sum(weights)
  do.call(cbind, Map(function(code, weight) code * sqrt(weight), fit$codes, weights))
}

#' Form and compare candidate hard partitions of SOM units
#'
#' @param ensemble A fitted `som_ensemble`.
#' @param k Candidate numbers of clusters. Defaults to the ensemble specification.
#' @param method Hierarchical clustering method used on SOM codebooks.
#' @param scope Evidence scope. The default, `"analysis"`, masks every row not
#'   used to fit a given ensemble member. `"all"` explicitly audits the
#'   stability of mapped labels for all rows and must not be used as training-
#'   partition evidence in [assess_defensibility()].
#'
#' @return A `som_partitions` object. Agreement is reported as ARI, not accuracy.
#' @export
partition_som <- function(
  ensemble,
  k = ensemble$spec$k,
  method = "ward.D2",
  scope = c("analysis", "all")
) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`ensemble` must come from `fit_som_ensemble()`.")
  }
  scope <- match.arg(scope)
  if (!is.numeric(k) || !length(k) || anyNA(k) || any(k < 2) ||
        any(k %% 1 != 0)) {
    .abort("`k` must contain integers of at least two.")
  }
  k <- sort(unique(as.integer(k)))
  successful <- Filter(function(x) isTRUE(x$success), ensemble$fits)
  if (!length(successful)) {
    return(structure(
      list(
        records = list(),
        pairwise = data.frame(
          k = integer(), fit_a = character(), fit_b = character(),
          scope = character(), n_joint = integer(), joint_coverage = numeric(),
          ari = numeric(), ami = numeric(), stringsAsFactors = FALSE
        ),
        stability = data.frame(
          k = integer(), scope = character(), n_pairs = integer(),
          n_pairs_evaluable = integer(), median_joint_n = numeric(),
          median_joint_coverage = numeric(), median_ari = numeric(),
          ari_q025 = numeric(), ari_q975 = numeric(), median_ami = numeric(),
          ami_q025 = numeric(), ami_q975 = numeric(),
          stringsAsFactors = FALSE
        ),
        method = method,
        scope = scope,
        ensemble = ensemble
      ),
      class = "som_partitions"
    ))
  }
  unit_counts <- vapply(successful, function(fit) {
    nrow(fit$codes[[1L]])
  }, integer(1))
  minimum_units <- min(unit_counts)
  if (max(k) > minimum_units) {
    .abort(paste0(
      "Every candidate `k` must be available for every successful fit. ",
      "The smallest fitted grid has ", minimum_units, " units."
    ))
  }
  records <- list()
  cursor <- 0L
  for (fit in successful) {
    codes <- .weighted_codes(fit)
    tree <- stats::hclust(stats::dist(codes), method = method)
    for (candidate_k in k) {
      cursor <- cursor + 1L
      unit_labels <- stats::cutree(tree, k = candidate_k)
      mapped_labels <- as.integer(unit_labels[fit$bmu])
      sample_labels <- mapped_labels
      if (scope == "analysis") {
        sample_labels[-fit$analysis] <- NA_integer_
      }
      records[[cursor]] <- list(
        id = fit$id,
        split_id = fit$split_id,
        grid_id = fit$grid_id,
        k = candidate_k,
        analysis = fit$analysis,
        assessment = fit$assessment,
        unit_labels = unit_labels,
        mapped_labels = mapped_labels,
        sample_labels = sample_labels,
        scope = scope
      )
    }
  }

  pairwise <- list()
  cursor <- 0L
  for (candidate_k in sort(unique(k))) {
    subset <- Filter(function(z) z$k == candidate_k, records)
    if (length(subset) < 2L) next
    pairs <- utils::combn(seq_along(subset), 2L)
    for (j in seq_len(ncol(pairs))) {
      cursor <- cursor + 1L
      a <- subset[[pairs[1L, j]]]
      b <- subset[[pairs[2L, j]]]
      jointly_observed <- !is.na(a$sample_labels) & !is.na(b$sample_labels)
      pairwise[[cursor]] <- data.frame(
        k = candidate_k,
        fit_a = a$id,
        fit_b = b$id,
        scope = scope,
        n_joint = sum(jointly_observed),
        joint_coverage = mean(jointly_observed),
        ari = .adjusted_rand(a$sample_labels, b$sample_labels),
        ami = .adjusted_mutual_info(a$sample_labels, b$sample_labels),
        stringsAsFactors = FALSE
      )
    }
  }
  pairwise_table <- if (length(pairwise)) {
    do.call(rbind, pairwise)
  } else {
    data.frame(
      k = integer(), fit_a = character(), fit_b = character(),
      scope = character(), n_joint = integer(), joint_coverage = numeric(),
      ari = numeric(), ami = numeric(), stringsAsFactors = FALSE
    )
  }

  stability <- if (nrow(pairwise_table)) {
    do.call(rbind, lapply(split(pairwise_table, pairwise_table$k), function(z) {
      interval <- .quantile_safe(z$ari, c(0.025, 0.975))
      ami_interval <- .quantile_safe(z$ami, c(0.025, 0.975))
      finite_ari <- z$ari[is.finite(z$ari)]
      finite_ami <- z$ami[is.finite(z$ami)]
      data.frame(
        k = z$k[[1L]],
        scope = z$scope[[1L]],
        n_pairs = nrow(z),
        n_pairs_evaluable = sum(is.finite(z$ari) & is.finite(z$ami)),
        median_joint_n = stats::median(z$n_joint),
        median_joint_coverage = stats::median(z$joint_coverage),
        median_ari = if (length(finite_ari)) stats::median(finite_ari) else NA_real_,
        ari_q025 = interval[[1L]],
        ari_q975 = interval[[2L]],
        median_ami = if (length(finite_ami)) stats::median(finite_ami) else NA_real_,
        ami_q025 = ami_interval[[1L]],
        ami_q975 = ami_interval[[2L]],
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      k = integer(), scope = character(), n_pairs = integer(),
      n_pairs_evaluable = integer(), median_joint_n = numeric(),
      median_joint_coverage = numeric(),
      median_ari = numeric(),
      ari_q025 = numeric(), ari_q975 = numeric(), median_ami = numeric(),
      ami_q025 = numeric(), ami_q975 = numeric()
    )
  }

  structure(
    list(
      records = records, pairwise = pairwise_table, stability = stability,
      method = method, scope = scope, ensemble = ensemble
    ),
    class = "som_partitions"
  )
}

#' @export
print.som_partitions <- function(x, ...) {
  candidate_k <- sort(unique(vapply(x$records, `[[`, integer(1), "k")))
  cat("<som_partitions>\n")
  cat("  fitted partitions :", length(x$records), "\n")
  cat("  candidate k       :", paste(candidate_k, collapse = ", "), "\n")
  cat("  evidence scope    :", x$scope, "\n")
  cat("  pairwise contrasts:", nrow(x$pairwise), "\n")
  cat("  agreement metric  : adjusted Rand index (not accuracy)\n")
  invisible(x)
}
