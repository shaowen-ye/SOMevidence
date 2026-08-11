.abort <- function(message, call = NULL) {
  stop(message, call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.count_noun <- function(n, singular, plural = paste0(singular, "s")) {
  n <- as.integer(n)
  sprintf("%d %s", n, if (n == 1L) singular else plural)
}

.with_reproducible_seed <- function(seed, code) {
  withr::with_seed(
    seed,
    code,
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
}

.capture_warnings <- function(code) {
  warnings <- list()
  value <- tryCatch(
    withCallingHandlers(
      code,
      warning = function(condition) {
        warnings[[length(warnings) + 1L]] <<- data.frame(
          warning_class = class(condition)[[1L]],
          warning = conditionMessage(condition),
          stringsAsFactors = FALSE
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) condition
  )
  warning_table <- if (length(warnings)) {
    do.call(rbind, warnings)
  } else {
    data.frame(
      warning_class = character(), warning = character(),
      stringsAsFactors = FALSE
    )
  }
  list(value = value, warnings = warning_table)
}

.assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .abort(sprintf("`%s` must be TRUE or FALSE.", name))
  }
  invisible(x)
}

.assert_scalar_number <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
        x < lower || x > upper) {
    .abort(sprintf(
      "`%s` must be one number in [%s, %s].",
      name, format(lower), format(upper)
    ))
  }
  invisible(x)
}

.assert_scalar_integer <- function(x, name, lower = -.Machine$integer.max,
                                   upper = .Machine$integer.max) {
  .assert_scalar_number(x, name, lower = lower, upper = upper)
  if (x %% 1 != 0) {
    .abort(sprintf("`%s` must be an integer.", name))
  }
  invisible(x)
}

.assert_integer_vector <- function(x, name, lower = -.Machine$integer.max,
                                   upper = .Machine$integer.max,
                                   allow_empty = FALSE) {
  if (!is.numeric(x) || (!allow_empty && !length(x)) || anyNA(x) ||
        any(!is.finite(x)) || any(x %% 1 != 0) ||
        any(x < lower | x > upper)) {
    qualifier <- if (allow_empty) "zero or more" else "one or more"
    .abort(sprintf(
      "`%s` must contain %s integers in [%s, %s].",
      name, qualifier, format(lower), format(upper)
    ))
  }
  invisible(x)
}

.seed_from_key <- function(seed, ...) {
  .assert_scalar_integer(seed, "seed", lower = 0)
  key <- paste(..., collapse = "::")
  code_points <- utf8ToInt(enc2utf8(key))
  if (!length(code_points)) return(as.integer(seed))
  weights <- (seq_along(code_points) %% 104729L) + 1L
  modulus <- .Machine$integer.max - 1
  value <- (as.double(seed) + sum(as.double(code_points) * weights)) %% modulus
  as.integer(value + 1)
}

.as_numeric_matrix <- function(x, name) {
  if (is.data.frame(x)) {
    non_numeric <- !vapply(x, is.numeric, logical(1))
    if (any(non_numeric)) {
      .abort(sprintf(
        "Layer `%s` contains non-numeric columns: %s.",
        name, paste(names(x)[non_numeric], collapse = ", ")
      ))
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x) || !is.numeric(x)) {
    .abort(sprintf("Layer `%s` must be a numeric matrix or data frame.", name))
  }
  storage.mode(x) <- "double"
  if (nrow(x) < 1L || ncol(x) < 1L) {
    .abort(sprintf("Layer `%s` must have at least one row and one column.", name))
  }
  if (any(is.infinite(x))) {
    .abort(sprintf("Layer `%s` contains infinite values.", name))
  }
  x
}

.validate_index <- function(index, n, name) {
  if (!is.numeric(index) || anyNA(index) || any(index %% 1 != 0) ||
        any(index < 1L | index > n)) {
    .abort(sprintf("`%s` must contain valid row indices.", name))
  }
  unique(as.integer(index))
}

.quantile_safe <- function(x, probs) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(rep(NA_real_, length(probs)))
  }
  as.numeric(stats::quantile(x, probs = probs, names = FALSE, type = 8))
}

.unoccupied_rate <- function(mapped_units, occupied_units) {
  observed <- !is.na(mapped_units)
  if (!any(observed)) return(NA_real_)
  mean(!mapped_units[observed] %in% occupied_units)
}

.resolve_layer_weights <- function(layers, layer_weights, normalize_layers) {
  weights <- layer_weights %||% rep(1, length(layers))
  if (!is.null(names(weights))) weights <- weights[names(layers)]
  if (length(weights) != length(layers) || anyNA(weights) ||
        any(!is.finite(weights)) ||
        any(weights < 0) || sum(weights) <= 0) {
    .abort("Layer weights must provide one non-negative value per layer.")
  }
  requested <- weights / sum(weights)

  mean_squared_distance <- vapply(layers, function(x) {
    variances <- apply(x, 2L, stats::var, na.rm = TRUE)
    if (any(!is.finite(variances))) {
      return(NA_real_)
    }
    2 * sum(variances)
  }, numeric(1))
  if (normalize_layers && any(
    !is.finite(mean_squared_distance) |
      mean_squared_distance <= .Machine$double.eps
  )) {
    .abort("Layer normalization requires informative training data in every layer.")
  }

  raw_effective <- if (normalize_layers) {
    requested / mean_squared_distance
  } else {
    requested
  }
  effective <- raw_effective / sum(raw_effective)
  names(requested) <- names(mean_squared_distance) <- names(effective) <-
    names(layers)
  list(
    requested = requested,
    mean_squared_distance = mean_squared_distance,
    effective = effective
  )
}
