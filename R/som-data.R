#' Construct a design-explicit SOM data object
#'
#' @param x A numeric matrix or data frame for a single-layer analysis.
#' @param layers A named list of numeric matrices or data frames. Use either
#'   `x` or `layers`, not both.
#' @param id Optional unique sample identifiers.
#' @param group Optional sampling-group identifiers.
#' @param time Optional time values retained as metadata.
#' @param domain Optional monitoring-domain identifiers.
#' @param weight Optional non-negative survey or summary weights. These weights
#'   are retained for design summaries and are not silently used as SOM
#'   training weights.
#' @param external_label Optional external ecological labels. They are retained
#'   for post hoc assessment and never added to the training layers.
#'
#' @return An object of class `som_data`. The scalar `id_source` records
#'   whether sample identifiers were supplied, taken from explicit
#'   non-positional row names or generated locally. Stable identifiers are
#'   required to compare different data-coverage scenarios.
#' @export
som_data <- function(x = NULL, layers = NULL, id = NULL, group = NULL,
                     time = NULL, domain = NULL, weight = NULL,
                     external_label = NULL) {
  if (is.null(x) == is.null(layers)) {
    .abort("Supply exactly one of `x` or `layers`.")
  }

  if (!is.null(x)) {
    layers <- list(data = x)
  }
  if (!is.list(layers) || !length(layers)) {
    .abort("`layers` must be a non-empty named list.")
  }
  if (is.null(names(layers)) || any(names(layers) == "") ||
        anyDuplicated(names(layers))) {
    .abort("Every layer must have a unique, non-empty name.")
  }

  source_layer <- layers[[1L]]
  rectangular_source <- is.matrix(source_layer) || is.data.frame(source_layer)
  source_rownames <- if (rectangular_source) rownames(source_layer) else NULL
  positional_rownames <- rectangular_source && identical(
    as.character(source_rownames), as.character(seq_len(nrow(source_layer)))
  )
  generated_rownames <- !is.null(source_rownames) && length(source_rownames) &&
    any(grepl("^(sample|simulation)_[0-9]+$", source_rownames))
  has_stable_rownames <- !is.null(source_rownames) && !(
    is.data.frame(source_layer) && .row_names_info(source_layer, type = 1L) < 0L
  ) && !positional_rownames && !generated_rownames
  if (is.null(id)) {
    if (has_stable_rownames) {
      id <- source_rownames
      id_source <- "rownames"
    } else {
      id_source <- "generated"
    }
  } else {
    id_source <- "provided"
  }

  layers <- Map(.as_numeric_matrix, layers, names(layers))
  for (nm in names(layers)) {
    columns <- colnames(layers[[nm]])
    if (is.null(columns)) {
      colnames(layers[[nm]]) <- sprintf("%s_%02d", nm, seq_len(ncol(layers[[nm]])))
    } else if (anyNA(columns) || any(columns == "") || anyDuplicated(columns)) {
      .abort(sprintf(
        "Layer `%s` must have unique, non-empty variable names.",
        nm
      ))
    }
  }
  n <- nrow(layers[[1L]])
  layer_rows <- vapply(layers, nrow, integer(1))
  if (any(layer_rows != n)) {
    .abort("All layers must contain the same samples in the same row order.")
  }

  id <- id %||% sprintf("sample_%05d", seq_len(n))
  if (length(id) != n || anyNA(id) || anyDuplicated(id)) {
    .abort("`id` must contain one unique, non-missing value per row.")
  }
  id <- as.character(id)
  for (i in seq_along(layers)) rownames(layers[[i]]) <- id

  check_length <- function(value, name) {
    if (!is.null(value) && length(value) != n) {
      .abort(sprintf("`%s` must have length %d.", name, n))
    }
    value
  }
  group <- check_length(group, "group")
  time <- check_length(time, "time")
  domain <- check_length(domain, "domain")
  weight <- check_length(weight, "weight")
  external_label <- check_length(external_label, "external_label")

  if (!is.null(weight) &&
        (!is.numeric(weight) || anyNA(weight) || any(weight < 0))) {
    .abort("`weight` must be numeric, non-missing and non-negative.")
  }

  metadata <- data.frame(id = id, stringsAsFactors = FALSE)
  if (!is.null(group)) metadata$group <- group
  if (!is.null(time)) metadata$time <- time
  if (!is.null(domain)) metadata$domain <- domain
  if (!is.null(weight)) metadata$weight <- weight
  if (!is.null(external_label)) metadata$external_label <- external_label

  structure(
    list(layers = layers, metadata = metadata, id_source = id_source),
    class = "som_data"
  )
}

#' @export
print.som_data <- function(x, ...) {
  cat("<som_data>\n")
  cat("  samples:", nrow(x$metadata), "\n")
  cat("  layers :", length(x$layers), "\n")
  cat("  id source:", x$id_source %||% "unknown", "\n")
  for (nm in names(x$layers)) {
    m <- x$layers[[nm]]
    cat(sprintf(
      "    - %s: %d variables, %.1f%% missing\n",
      nm, ncol(m), 100 * mean(is.na(m))
    ))
  }
  design_fields <- intersect(
    c("group", "time", "domain", "weight", "external_label"),
    names(x$metadata)
  )
  cat("  design :", if (length(design_fields)) {
    paste(design_fields, collapse = ", ")
  } else {
    "sample id only"
  }, "\n")
  invisible(x)
}
