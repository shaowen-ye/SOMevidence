.v112_same_partition_from_table <- function(tab) {
  tab <- tab[
    rowSums(tab) > 0,
    colSums(tab) > 0,
    drop = FALSE
  ]
  occupied <- tab > 0L
  all(rowSums(occupied) == 1L) && all(colSums(occupied) == 1L)
}

.v112_adjusted_rand <- function(x, y) {
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
    return(if (.v112_same_partition_from_table(tab)) 1 else 0)
  }
  (index - expected) / denominator
}

.v112_label_entropy <- function(x) {
  probability <- as.numeric(table(x)) / length(x)
  probability <- probability[probability > 0]
  -sum(probability * log(probability))
}

.v112_expected_mutual_information <- function(tab) {
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

.v112_adjusted_mutual_info <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  tab <- table(x, y)
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
  expected <- .v112_expected_mutual_information(tab)
  normalizer <- mean(c(.v112_label_entropy(x), .v112_label_entropy(y)))
  denominator <- normalizer - expected
  if (abs(denominator) < sqrt(.Machine$double.eps)) {
    return(if (.v112_same_partition_from_table(tab)) 1 else 0)
  }
  value <- (mutual_information - expected) / denominator
  max(-1, min(1, value))
}

.v112_model_distance_matrix <- function(fit) {
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
      layer_d[valid, u] <- rowSums(
        delta[valid, , drop = FALSE]^2,
        na.rm = TRUE
      ) * p / observed_n[valid]
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

.v112_topographic_error <- function(fit, rows) {
  if (!isTRUE(fit$success)) {
    return(NA_real_)
  }
  distance_matrix <- .v112_model_distance_matrix(fit)[
    rows,
    ,
    drop = FALSE
  ]
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

.v113_restore_processed_layers <- function(fit, data) {
  fit$processed_all <- Map(
    function(layer, fitted) {
      SOMevidence:::.apply_preprocessor(layer, fitted)
    },
    data$layers,
    fit$fitted_preprocess
  )
  fit
}
