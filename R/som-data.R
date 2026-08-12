#' Construct a design-explicit SOM data object
#'
#' @param x A numeric matrix or data frame for a single-layer analysis.
#' @param layers A named list of numeric matrices or data frames. Use either
#'   `x` or `layers`, not both.
#' @param id Optional unique sample identifiers. Supplying `id` explicitly is
#'   recommended for scientific analyses. If it is omitted, informative row
#'   names are used unless any row name follows the package-like
#'   `sample_<integer>` or `simulation_<integer>` pattern. Mixed provenance
#'   cannot be inferred safely, so such row-name vectors are treated as
#'   generated; pass `id` explicitly to confirm intentional identifiers.
#' @param group Optional sampling-group identifiers.
#' @param time Optional time values retained as metadata.
#' @param domain Optional monitoring-domain identifiers.
#' @param weight Optional non-negative survey or summary weights. These weights
#'   are retained only as metadata in the current release. They are not used
#'   in SOM training, evidence metrics or design summaries.
#' @param external_label Optional external ecological labels. They are retained
#'   for post hoc assessment and never added to the training layers. A fully
#'   named vector is aligned to `id`; partial, duplicate or unmatched label IDs
#'   are rejected. An unnamed vector is interpreted positionally.
#'
#' @return An object of class `som_data`. Named multi-layer inputs are aligned
#'   to the first layer by row name when all layers provide row names;
#'   incompatible or partly named layers are rejected. An explicit `id` vector
#'   is always interpreted positionally against the first layer and never
#'   changes its row order. If no layers have usable row names, their existing
#'   row order is treated as authoritative. The scalar `id_source` records
#'   whether sample identifiers were supplied, taken from explicit
#'   non-positional row names or generated locally. Stable identifiers are
#'   required to compare different data-coverage scenarios. A mixture of
#'   generic and study-specific row names is conservatively treated as
#'   generated unless the intended identifiers are supplied through `id`.
#'   This provenance label does not disable multi-layer row-name alignment:
#'   when every layer has row names, later layers are still matched to the
#'   first layer by those names. If such layer row names are not trustworthy,
#'   first place every layer in the same row order, remove its row names, and
#'   pass the confirmed identifiers through `id`.
#' @examples
#' x <- data.frame(temperature = 10:14, oxygen = c(9, 8, 8, 7, 6))
#' data <- som_data(x, id = paste0("sample_", seq_len(nrow(x))))
#' data
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

  row_identity <- lapply(layers, function(layer) {
    rectangular <- is.matrix(layer) || is.data.frame(layer)
    rn <- if (rectangular) rownames(layer) else NULL
    positional <- rectangular && identical(
      as.character(rn), as.character(seq_len(nrow(layer)))
    )
    generated <- !is.null(rn) && length(rn) &&
      any(grepl("^(sample|simulation)_[0-9]+$", rn))
    automatic <- rectangular && is.data.frame(layer) &&
      .row_names_info(layer, type = 1L) < 0L
    comparable <- !is.null(rn) && !automatic && !positional
    informative <- comparable && !generated
    list(
      names = as.character(rn), comparable = comparable,
      informative = informative
    )
  })
  source_rownames <- row_identity[[1L]]$names
  has_stable_rownames <- isTRUE(row_identity[[1L]]$informative)
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
  if (length(id) != n || anyNA(id) || anyDuplicated(id) ||
        any(!nzchar(trimws(as.character(id))))) {
    .abort("`id` must contain one unique, non-empty value per row.")
  }
  id <- as.character(id)

  comparable <- vapply(row_identity, `[[`, logical(1), "comparable")
  if (length(layers) > 1L && any(comparable) && !all(comparable)) {
    .abort(paste0(
      "Multi-layer row identity is ambiguous: either every layer must have ",
      "unique sample row names or none may rely on row names."
    ))
  }
  if (length(layers) > 1L && all(comparable)) {
    alignment_ids <- row_identity[[1L]]$names
    for (i in seq_along(layers)) {
      row_ids <- row_identity[[i]]$names
      if (length(row_ids) != n || anyNA(row_ids) || any(!nzchar(row_ids)) ||
            anyDuplicated(row_ids)) {
        .abort(sprintf(
          "Layer `%s` must have unique, non-empty sample row names.",
          names(layers)[[i]]
        ))
      }
      if (!setequal(row_ids, alignment_ids)) {
        .abort(sprintf(
          "Layer `%s` row names do not match the other layer sample IDs.",
          names(layers)[[i]]
        ))
      }
      layers[[i]] <- layers[[i]][
        match(alignment_ids, row_ids), , drop = FALSE
      ]
    }
  }
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

  external_label_source <- if (is.null(external_label)) {
    "none"
  } else if (is.null(names(external_label))) {
    "position"
  } else {
    "named_id"
  }
  if (identical(external_label_source, "named_id")) {
    external_ids <- as.character(names(external_label))
    if (length(external_ids) != n || anyNA(external_ids) ||
          any(!nzchar(trimws(external_ids))) || anyDuplicated(external_ids)) {
      .abort(paste0(
        "Named `external_label` values must have one unique, non-empty ",
        "sample ID per label."
      ))
    }
    if (!setequal(external_ids, id)) {
      .abort("Named `external_label` IDs must match `id` exactly.")
    }
    external_label <- external_label[match(id, external_ids)]
  }

  if (!is.null(weight) &&
        (!is.numeric(weight) || anyNA(weight) || any(!is.finite(weight)) ||
           any(weight < 0))) {
    .abort("`weight` must be numeric, non-missing and non-negative.")
  }

  metadata <- data.frame(id = id, stringsAsFactors = FALSE)
  if (!is.null(group)) metadata$group <- group
  if (!is.null(time)) metadata$time <- time
  if (!is.null(domain)) metadata$domain <- domain
  if (!is.null(weight)) metadata$weight <- weight
  if (!is.null(external_label)) metadata$external_label <- external_label

  .new_som_object(
    list(
      layers = layers,
      metadata = metadata,
      id_source = id_source,
      external_label_source = external_label_source
    ),
    "som_data"
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
