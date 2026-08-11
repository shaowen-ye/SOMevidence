.model_distance_matrix <- function(fit, rows = NULL) {
  layers <- fit$processed_all
  if (!is.null(rows)) {
    layers <- lapply(layers, function(layer) {
      layer[rows, , drop = FALSE]
    })
  }
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
  distance_matrix <- .model_distance_matrix(fit, rows = rows)
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
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 1)
#' specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
#' audit_som(fit_som_ensemble(data, specification, keep_models = FALSE))
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
      grid = character(), xdim = integer(), ydim = integer(),
      n_fits = integer(),
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
      topographic_error = fit$training_topographic_error %||%
        .topographic_error(fit, fit$analysis),
      empty_unit_rate = fit$empty_unit_rate,
      stringsAsFactors = FALSE
    )
  }))

  key <- interaction(fit_metrics$xdim, fit_metrics$ydim, drop = TRUE)
  summary_rows <- lapply(split(fit_metrics, key), function(z) {
    data.frame(
      grid = sprintf("%d x %d", z$xdim[[1L]], z$ydim[[1L]]),
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

  grid_summary <- do.call(rbind, summary_rows)
  rownames(grid_summary) <- NULL
  structure(
    list(
      fit_metrics = fit_metrics,
      grid_summary = grid_summary,
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

.same_partition_from_table <- function(tab) {
  tab <- tab[
    rowSums(tab) > 0,
    colSums(tab) > 0,
    drop = FALSE
  ]
  occupied <- tab > 0L
  all(rowSums(occupied) == 1L) && all(colSums(occupied) == 1L)
}

.partition_contingency <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  list(
    x = x,
    y = y,
    n = length(x),
    table = if (length(x) >= 2L) table(x, y) else NULL
  )
}

.ari_from_contingency <- function(contingency) {
  if (contingency$n < 2L) {
    return(NA_real_)
  }
  tab <- contingency$table
  choose2 <- function(z) z * (z - 1) / 2
  index <- sum(choose2(tab))
  row_pairs <- sum(choose2(rowSums(tab)))
  col_pairs <- sum(choose2(colSums(tab)))
  total_pairs <- choose2(contingency$n)
  expected <- row_pairs * col_pairs / total_pairs
  maximum <- 0.5 * (row_pairs + col_pairs)
  denominator <- maximum - expected
  if (abs(denominator) < .Machine$double.eps) {
    return(if (.same_partition_from_table(tab)) 1 else 0)
  }
  (index - expected) / denominator
}

.adjusted_rand <- function(x, y) {
  .ari_from_contingency(.partition_contingency(x, y))
}

.label_entropy <- function(x) {
  probability <- as.numeric(table(x)) / length(x)
  probability <- probability[probability > 0]
  -sum(probability * log(probability))
}

.expected_mutual_information <- function(tab) {
  n <- as.double(sum(tab))
  row_n <- as.double(rowSums(tab))
  col_n <- as.double(colSums(tab))
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

.ami_from_contingency <- function(contingency) {
  if (contingency$n < 2L) {
    return(NA_real_)
  }
  tab <- contingency$table
  tab <- tab[
    rowSums(tab) > 0,
    colSums(tab) > 0,
    drop = FALSE
  ]
  n <- as.double(sum(tab))
  nonzero <- tab > 0
  row_n <- as.double(rowSums(tab))
  col_n <- as.double(colSums(tab))
  observed <- as.double(tab[nonzero])
  mutual_information <- sum(
    (observed / n) * log(
      n * observed /
        outer(row_n, col_n)[nonzero]
    )
  )
  expected <- .expected_mutual_information(tab)
  normalizer <- mean(c(
    .label_entropy(contingency$x),
    .label_entropy(contingency$y)
  ))
  denominator <- normalizer - expected
  if (abs(denominator) < sqrt(.Machine$double.eps)) {
    return(if (.same_partition_from_table(tab)) 1 else 0)
  }
  value <- (mutual_information - expected) / denominator
  max(-1, min(1, value))
}

.adjusted_mutual_info <- function(x, y) {
  .ami_from_contingency(.partition_contingency(x, y))
}

.partition_agreement <- function(x, y) {
  contingency <- .partition_contingency(x, y)
  c(
    ari = .ari_from_contingency(contingency),
    ami = .ami_from_contingency(contingency)
  )
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
#' @param max_pairwise_comparisons Maximum number of pairwise partition
#'   comparisons created across all requested `k`. This explicit budget guards
#'   against quadratic growth in the number of ensemble members.
#'
#' @return A `som_partitions` object. Agreement is reported as ARI, not accuracy.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 2)
#' specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
#' ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
#' partition_som(ensemble)
#' @export
partition_som <- function(
  ensemble,
  k = ensemble$spec$k,
  method = "ward.D2",
  scope = c("analysis", "all"),
  max_pairwise_comparisons = 1000000L
) {
  if (!inherits(ensemble, "som_ensemble")) {
    .abort("`ensemble` must come from `fit_som_ensemble()`.")
  }
  scope <- match.arg(scope)
  .assert_integer_vector(k, "k", lower = 2)
  .assert_scalar_integer(
    max_pairwise_comparisons, "max_pairwise_comparisons", lower = 1
  )
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
          n_partitions = integer(), n_complete_partitions = integer(),
          min_observed_clusters = integer(),
          n_pairs_evaluable = integer(), median_joint_n = numeric(),
          median_joint_coverage = numeric(), median_ari = numeric(),
          ari_q025 = numeric(), ari_q975 = numeric(), median_ami = numeric(),
          ami_q025 = numeric(), ami_q975 = numeric(),
          stringsAsFactors = FALSE
        ),
        method = method,
        partition_method = method,
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
  requested_pairs <- choose(length(successful), 2) * length(k)
  if (requested_pairs > max_pairwise_comparisons) {
    .abort(paste0(
      "The requested audit would create ", format(requested_pairs),
      " pairwise comparisons, exceeding `max_pairwise_comparisons = ",
      format(max_pairwise_comparisons), "`. Reduce the model budget or ",
      "increase the limit deliberately."
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
      observed_clusters <- sort(unique(stats::na.omit(sample_labels)))
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
        n_observed_clusters = length(observed_clusters),
        observed_clusters = as.integer(observed_clusters),
        missing_clusters = setdiff(seq_len(candidate_k), observed_clusters),
        complete_k = length(observed_clusters) == candidate_k,
        scope = scope
      )
    }
  }

  pairwise_n <- as.integer(requested_pairs)
  pairwise_k <- integer(pairwise_n)
  pairwise_fit_a <- character(pairwise_n)
  pairwise_fit_b <- character(pairwise_n)
  pairwise_n_joint <- integer(pairwise_n)
  pairwise_joint_coverage <- numeric(pairwise_n)
  pairwise_ari <- numeric(pairwise_n)
  pairwise_ami <- numeric(pairwise_n)
  cursor <- 0L
  for (candidate_k in sort(unique(k))) {
    subset <- Filter(function(z) z$k == candidate_k, records)
    if (length(subset) < 2L) next
    for (first_index in seq_len(length(subset) - 1L)) {
      for (second_index in seq.int(first_index + 1L, length(subset))) {
        cursor <- cursor + 1L
        a <- subset[[first_index]]
        b <- subset[[second_index]]
        agreement <- .partition_agreement(
          a$sample_labels,
          b$sample_labels
        )
        jointly_observed <- !is.na(a$sample_labels) & !is.na(b$sample_labels)
        pairwise_k[[cursor]] <- candidate_k
        pairwise_fit_a[[cursor]] <- a$id
        pairwise_fit_b[[cursor]] <- b$id
        pairwise_n_joint[[cursor]] <- sum(jointly_observed)
        pairwise_joint_coverage[[cursor]] <- mean(jointly_observed)
        pairwise_ari[[cursor]] <- agreement[["ari"]]
        pairwise_ami[[cursor]] <- agreement[["ami"]]
      }
    }
  }
  pairwise_table <- if (cursor) {
    used <- seq_len(cursor)
    data.frame(
      k = pairwise_k[used],
      fit_a = pairwise_fit_a[used],
      fit_b = pairwise_fit_b[used],
      scope = rep(scope, cursor),
      n_joint = pairwise_n_joint[used],
      joint_coverage = pairwise_joint_coverage[used],
      ari = pairwise_ari[used],
      ami = pairwise_ami[used],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      k = integer(), fit_a = character(), fit_b = character(),
      scope = character(), n_joint = integer(), joint_coverage = numeric(),
      ari = numeric(), ami = numeric(), stringsAsFactors = FALSE
    )
  }

  stability <- do.call(rbind, lapply(k, function(candidate_k) {
    z <- pairwise_table[pairwise_table$k == candidate_k, , drop = FALSE]
    candidate_records <- Filter(function(record) {
      record$k == candidate_k
    }, records)
    if (nrow(z)) {
      interval <- .quantile_safe(z$ari, c(0.025, 0.975))
      ami_interval <- .quantile_safe(z$ami, c(0.025, 0.975))
      finite_ari <- z$ari[is.finite(z$ari)]
      finite_ami <- z$ami[is.finite(z$ami)]
    } else {
      interval <- ami_interval <- c(NA_real_, NA_real_)
      finite_ari <- finite_ami <- numeric()
    }
    data.frame(
      k = candidate_k,
      scope = scope,
      n_pairs = nrow(z),
      n_partitions = length(candidate_records),
      n_complete_partitions = sum(vapply(
        candidate_records, `[[`, logical(1), "complete_k"
      )),
      min_observed_clusters = min(vapply(
        candidate_records, `[[`, integer(1), "n_observed_clusters"
      )),
      n_pairs_evaluable = sum(is.finite(z$ari) & is.finite(z$ami)),
      median_joint_n = if (nrow(z)) stats::median(z$n_joint) else NA_real_,
      median_joint_coverage = if (nrow(z)) {
        stats::median(z$joint_coverage)
      } else {
        NA_real_
      },
      median_ari = if (length(finite_ari)) stats::median(finite_ari) else NA_real_,
      ari_q025 = interval[[1L]],
      ari_q975 = interval[[2L]],
      median_ami = if (length(finite_ami)) stats::median(finite_ami) else NA_real_,
      ami_q025 = ami_interval[[1L]],
      ami_q975 = ami_interval[[2L]],
      stringsAsFactors = FALSE
    )
  }))
  rownames(stability) <- NULL

  structure(
    list(
      records = records, pairwise = pairwise_table, stability = stability,
      method = method, partition_method = method, scope = scope,
      max_pairwise_comparisons = as.integer(max_pairwise_comparisons),
      ensemble = ensemble
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
