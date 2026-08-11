#' Define leakage-safe SOM preprocessing
#'
#' @param transform One of `"identity"`, `"log"`, `"log1p"`, `"sqrt"`,
#'   `"hellinger"` or `"clr"`. `"log"` denotes the natural logarithm. A
#'   vector can assign `"identity"`, `"log1p"`, `"log"` or `"sqrt"` by
#'   column; name that vector to protect against
#'   column-order changes. Hellinger and CLR are whole-matrix transformations
#'   and must be specified alone.
#' @param center Whether to subtract training-set column means.
#' @param scale Whether to divide by training-set column standard deviations.
#' @param zero_replacement Positive replacement required when `transform =
#'   "clr"` and zeros are present.
#'
#' @return A preprocessing specification of class `som_preprocess`.
#' @examples
#' som_preprocess(transform = c("log", "identity"))
#' @export
som_preprocess <- function(
  transform = "identity",
  center = TRUE,
  scale = TRUE,
  zero_replacement = NULL
) {
  allowed <- c("identity", "log", "log1p", "sqrt", "hellinger", "clr")
  if (!is.character(transform) || !length(transform) || anyNA(transform) ||
        any(!transform %in% allowed)) {
    .abort("`transform` contains an unsupported transformation.")
  }
  if (length(transform) > 1L && any(transform %in% c("hellinger", "clr"))) {
    .abort("Hellinger and CLR must be specified as whole-matrix transformations.")
  }
  .assert_flag(center, "center")
  .assert_flag(scale, "scale")
  if (!is.null(zero_replacement)) {
    .assert_scalar_number(
      zero_replacement, "zero_replacement",
      lower = .Machine$double.eps
    )
  }
  .new_som_object(
    list(
      transform = transform,
      center = center,
      scale = scale,
      zero_replacement = zero_replacement
    ),
    "som_preprocess"
  )
}

.column_transforms <- function(x, transform) {
  if (length(transform) != ncol(x)) {
    .abort("A columnwise `transform` must provide one value per variable.")
  }
  if (!is.null(names(transform))) {
    if (is.null(colnames(x)) || !setequal(names(transform), colnames(x))) {
      .abort("Named column transformations must match the matrix column names.")
    }
    transform <- transform[colnames(x)]
  }
  transformed <- x
  for (j in seq_len(ncol(x))) {
    observed <- x[, j][!is.na(x[, j])]
    if (transform[[j]] %in% c("log1p", "sqrt") && any(observed < 0)) {
      column <- if (is.null(colnames(x))) j else colnames(x)[[j]]
      .abort(sprintf(
        "`%s` transformation requires non-negative values in column `%s`.",
        transform[[j]], column
      ))
    }
    if (transform[[j]] == "log" && any(observed <= 0)) {
      column <- if (is.null(colnames(x))) j else colnames(x)[[j]]
      .abort(sprintf(
        "`log` transformation requires positive values in column `%s`.",
        column
      ))
    }
    if (transform[[j]] == "log") transformed[, j] <- log(x[, j])
    if (transform[[j]] == "log1p") transformed[, j] <- log1p(x[, j])
    if (transform[[j]] == "sqrt") transformed[, j] <- sqrt(x[, j])
  }
  transformed
}

.transform_matrix <- function(x, spec) {
  transform <- spec$transform
  if (length(transform) > 1L) {
    return(.column_transforms(x, transform))
  }
  if (transform == "identity") {
    return(x)
  }

  observed <- x[!is.na(x)]
  if (transform %in% c("log1p", "sqrt", "hellinger", "clr") &&
        any(observed < 0)) {
    .abort(sprintf("`%s` transformation requires non-negative values.", transform))
  }
  if (transform == "log1p") {
    return(log1p(x))
  }
  if (transform == "log") {
    if (any(observed <= 0)) {
      .abort("`log` transformation requires positive values.")
    }
    return(log(x))
  }
  if (transform == "sqrt") {
    return(sqrt(x))
  }

  if (anyNA(x)) {
    .abort(sprintf(
      paste0(
        "`%s` transformation does not infer the meaning of missing components; ",
        "handle missing values explicitly first."
      ),
      transform
    ))
  }
  row_totals <- rowSums(x)
  if (transform == "hellinger") {
    if (any(row_totals <= 0)) {
      .abort("Hellinger transformation requires every row to have a positive total.")
    }
    return(sqrt(x / row_totals))
  }

  if (any(x == 0)) {
    if (is.null(spec$zero_replacement)) {
      .abort("CLR transformation with zeros requires `zero_replacement`.")
    }
    x[x == 0] <- spec$zero_replacement
  }
  log_x <- log(x)
  log_x - rowMeans(log_x)
}

.fit_preprocessor <- function(x, spec) {
  transformed <- .transform_matrix(x, spec)
  means <- if (spec$center) {
    colMeans(transformed, na.rm = TRUE)
  } else {
    rep(0, ncol(transformed))
  }
  if (any(!is.finite(means))) {
    .abort("At least one variable has no observed training values.")
  }

  scales <- if (spec$scale) {
    apply(transformed, 2L, stats::sd, na.rm = TRUE)
  } else {
    rep(1, ncol(transformed))
  }
  constant <- !is.finite(scales) | scales == 0
  scales[constant] <- 1

  fitted <- structure(
    c(spec, list(means = means, scales = scales, constant = constant)),
    class = "som_fitted_preprocess"
  )
  list(
    data = sweep(sweep(transformed, 2L, means, "-"), 2L, scales, "/"),
    fitted = fitted
  )
}

.apply_preprocessor <- function(x, fitted) {
  transformed <- .transform_matrix(x, fitted)
  output <- sweep(
    sweep(transformed, 2L, fitted$means, "-"),
    2L, fitted$scales, "/"
  )
  if (any(fitted$constant)) {
    # A variable with no training-set variation has no estimable distance
    # scale. Exclude it from mapping distance instead of applying raw-unit
    # deviations to held-out data through an arbitrary scale of one.
    output[, fitted$constant] <- 0
  }
  output
}

.normalise_preprocess <- function(preprocess, layer_names) {
  if (inherits(preprocess, "som_preprocess")) {
    return(stats::setNames(rep(list(preprocess), length(layer_names)), layer_names))
  }
  if (!is.list(preprocess) || is.null(names(preprocess)) ||
        !setequal(names(preprocess), layer_names) ||
        !all(vapply(preprocess, inherits, logical(1), "som_preprocess"))) {
    .abort(
      "`preprocess` must be one `som_preprocess()` object or a named object per layer."
    )
  }
  preprocess[layer_names]
}
