#' Specify a reproducible SOM ensemble
#'
#' @param grids A two-column data frame with `xdim` and `ydim`, a numeric
#'   vector of length two for one grid, or a list of such vectors.
#' @param seeds Integer random seeds.
#' @param rlen Number of training iterations.
#' @param alpha Initial and final learning rates.
#' @param radius Optional initial and final neighbourhood radii.
#' @param topology Rectangular or hexagonal grid.
#' @param neighbourhood Bubble or Gaussian neighbourhood.
#' @param toroidal Whether the grid is toroidal.
#' @param mode Training mode supported by [kohonen::supersom()].
#' @param k Candidate numbers of hard partitions retained for later auditing.
#' @param max_na_fraction Maximum allowed fraction of missing variables per
#'   layer and sample passed to `kohonen`.
#' @param layer_weights Optional non-negative scientific weights, one per layer.
#' @param normalize_layers Whether to divide each scientific layer weight by
#'   its training-set mean squared Euclidean distance. The package uses the
#'   variance identity for a deterministic, leakage-safe estimate and applies
#'   the same effective weights to SOM and cross-model fits.
#' @param cores Number of cores used by `kohonen` in parallel batch mode.
#'
#' @return A `som_spec` object.
#' @export
som_spec <- function(grids = data.frame(xdim = 7L, ydim = 5L),
                     seeds = 1:10, rlen = 500L,
                     alpha = c(0.05, 0.01), radius = NULL,
                     topology = c("hexagonal", "rectangular"),
                     neighbourhood = c("gaussian", "bubble"),
                     toroidal = FALSE,
                     mode = c("batch", "online", "pbatch"),
                     k = 2:8, max_na_fraction = 0,
                     layer_weights = NULL, normalize_layers = TRUE,
                     cores = 1L) {
  topology <- match.arg(topology)
  neighbourhood <- match.arg(neighbourhood)
  mode <- match.arg(mode)
  .assert_flag(toroidal, "toroidal")
  .assert_flag(normalize_layers, "normalize_layers")

  if (is.list(grids) && !is.data.frame(grids)) {
    valid <- vapply(
      grids,
      function(x) is.numeric(x) && length(x) == 2L && !anyNA(x),
      logical(1)
    )
    if (!length(grids) || !all(valid)) {
      .abort("Every element of a grid list must contain `xdim` and `ydim`.")
    }
    grids <- data.frame(
      xdim = vapply(grids, `[[`, numeric(1), 1L),
      ydim = vapply(grids, `[[`, numeric(1), 2L)
    )
  }
  if (is.numeric(grids) && length(grids) == 2L) {
    grids <- data.frame(xdim = grids[[1L]], ydim = grids[[2L]])
  }
  if (!is.data.frame(grids) || !all(c("xdim", "ydim") %in% names(grids))) {
    .abort("`grids` must contain integer columns `xdim` and `ydim`.")
  }
  grids <- grids[, c("xdim", "ydim"), drop = FALSE]
  if (anyNA(grids) || any(as.matrix(grids) %% 1 != 0) ||
        any(as.matrix(grids) < 2)) {
    .abort("Every grid dimension must be an integer of at least two.")
  }
  grids$xdim <- as.integer(grids$xdim)
  grids$ydim <- as.integer(grids$ydim)
  grids <- unique(grids)

  if (!is.numeric(seeds) || !length(seeds) || anyNA(seeds) ||
        any(seeds %% 1 != 0)) {
    .abort("`seeds` must contain non-missing integers.")
  }
  seeds <- unique(as.integer(seeds))
  .assert_scalar_number(rlen, "rlen", lower = 1)
  if (length(alpha) != 2L || anyNA(alpha) || any(alpha <= 0)) {
    .abort("`alpha` must contain two positive numbers.")
  }
  if (!is.null(radius) &&
        (!is.numeric(radius) || !length(radius) || anyNA(radius) || any(radius < 0))) {
    .abort("`radius` must be NULL or a non-negative numeric vector.")
  }
  if (!is.numeric(k) || !length(k) || anyNA(k) || any(k %% 1 != 0) || any(k < 2)) {
    .abort("`k` must contain integers of at least two.")
  }
  k <- sort(unique(as.integer(k)))
  minimum_units <- min(grids$xdim * grids$ydim)
  if (max(k) > minimum_units) {
    .abort(paste0(
      "Every candidate `k` must not exceed the number of units in the ",
      "smallest grid (", minimum_units, "). Use a common model budget or ",
      "define a separate sensitivity scenario."
    ))
  }
  .assert_scalar_number(max_na_fraction, "max_na_fraction", 0, 1)
  if (!is.null(layer_weights) &&
        (!is.numeric(layer_weights) || anyNA(layer_weights) ||
           any(layer_weights < 0) || sum(layer_weights) <= 0)) {
    .abort("`layer_weights` must be non-negative with a positive sum.")
  }
  .assert_scalar_number(cores, "cores", lower = 1)

  structure(
    list(
      grids = grids,
      seeds = seeds,
      rlen = as.integer(rlen),
      alpha = alpha,
      radius = radius,
      topology = topology,
      neighbourhood = neighbourhood,
      toroidal = toroidal,
      mode = mode,
      k = k,
      max_na_fraction = max_na_fraction,
      layer_weights = layer_weights,
      normalize_layers = normalize_layers,
      cores = as.integer(cores)
    ),
    class = "som_spec"
  )
}

#' Expand an ensemble specification into its model budget
#'
#' @param spec A `som_spec` object.
#' @return A data frame with one row per grid and random seed.
#' @export
expand_som_spec <- function(spec) {
  if (!inherits(spec, "som_spec")) .abort("`spec` must come from `som_spec()`.")
  out <- merge(
    transform(spec$grids, grid_id = seq_len(nrow(spec$grids))),
    data.frame(seed = spec$seeds),
    by = NULL
  )
  out <- out[order(out$grid_id, out$seed), , drop = FALSE]
  rownames(out) <- NULL
  out$model_id <- sprintf(
    "g%02d_%dx%d_s%d", out$grid_id, out$xdim, out$ydim, out$seed
  )
  out
}

#' @export
print.som_spec <- function(x, ...) {
  budget <- expand_som_spec(x)
  cat("<som_spec>\n")
  cat("  grids :", nrow(x$grids), "\n")
  cat("  seeds :", length(x$seeds), "\n")
  cat("  models:", nrow(budget), "per resample\n")
  cat("  mode  :", x$mode, "\n")
  cat("  k     :", paste(x$k, collapse = ", "), "\n")
  invisible(x)
}
