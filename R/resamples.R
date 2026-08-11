#' Construct design-aware resamples
#'
#' @param data A `som_data` object.
#' @param method One of `"full"`, `"subsample"`, `"group_subsample"`,
#'   `"block_subsample"`, `"leave_domain_out"` or `"custom"`.
#' @param repeats Number of repeated subsamples.
#' @param prop Proportion of rows or sampling units retained for analysis.
#' @param seed Random seed used to generate all splits.
#' @param unit Metadata column name or vector defining groups or predefined
#'   blocks. `block_subsample` does not infer blocks from row order.
#' @param domain Metadata column name or vector defining transfer domains.
#' @param splits For `method = "custom"`, a list containing `analysis` and
#'   optional `assessment` row indices.
#'
#' @return A `som_resamples` object.
#' @examples
#' x <- matrix(seq_len(120), nrow = 30)
#' data <- som_data(x, group = rep(paste0("site_", 1:10), each = 3))
#' som_resamples(data, method = "group_subsample", repeats = 2, seed = 1)
#' @export
som_resamples <- function(data,
                          method = c(
                            "full", "subsample", "group_subsample",
                            "block_subsample", "leave_domain_out", "custom"
                          ),
                          repeats = 50L, prop = 0.8, seed = 1L,
                          unit = "group", domain = "domain", splits = NULL) {
  if (!inherits(data, "som_data")) .abort("`data` must come from `som_data()`.")
  method <- match.arg(method)
  n <- nrow(data$metadata)
  .assert_scalar_integer(repeats, "repeats", lower = 1)
  .assert_scalar_number(prop, "prop", lower = .Machine$double.eps, upper = 1)
  .assert_scalar_integer(seed, "seed", lower = 0)

  resolve_vector <- function(value, name) {
    if (is.character(value) && length(value) == 1L) {
      if (!value %in% names(data$metadata)) {
        .abort(sprintf("Metadata column `%s` was not found.", value))
      }
      value <- data$metadata[[value]]
    }
    if (length(value) != n || anyNA(value)) {
      .abort(sprintf("`%s` must define one non-missing value per sample.", name))
    }
    value
  }

  make_split <- function(id, analysis, assessment = setdiff(seq_len(n), analysis)) {
    if (!is.character(id) || length(id) != 1L || is.na(id) ||
          !nzchar(trimws(id))) {
      .abort("Every split `id` must be one non-empty character value.")
    }
    analysis <- .validate_index(analysis, n, "analysis")
    assessment <- .validate_index(assessment, n, "assessment")
    if (!length(analysis)) .abort("Every split needs at least one analysis row.")
    if (length(intersect(analysis, assessment))) {
      .abort("Analysis and assessment rows must not overlap.")
    }
    list(id = id, analysis = analysis, assessment = assessment)
  }

  if (method == "full") {
    out <- list(make_split("full", seq_len(n), integer()))
  } else if (method == "custom") {
    if (!is.list(splits) || !length(splits)) {
      .abort("`splits` must be a non-empty list for a custom design.")
    }
    out <- lapply(seq_along(splits), function(i) {
      s <- splits[[i]]
      if (!is.list(s) || is.null(s$analysis)) {
        .abort("Each custom split must contain `analysis` row indices.")
      }
      make_split(
        s$id %||% sprintf("custom_%03d", i),
        s$analysis,
        s$assessment %||% setdiff(seq_len(n), s$analysis)
      )
    })
  } else if (method == "leave_domain_out") {
    domain_values <- resolve_vector(domain, "domain")
    domains <- unique(domain_values)
    if (length(domains) < 2L) .abort("Leave-domain-out requires at least two domains.")
    out <- lapply(seq_along(domains), function(i) {
      held <- domains[[i]]
      split <- make_split(
        sprintf(
          "leave_domain_%03d_%s", i,
          make.names(as.character(held), unique = FALSE)
        ),
        which(domain_values != held),
        which(domain_values == held)
      )
      split$held_domain <- held
      split
    })
  } else {
    out <- .with_reproducible_seed(as.integer(seed), {
      lapply(seq_len(as.integer(repeats)), function(i) {
        if (method == "subsample") {
          size <- max(2L, floor(prop * n))
          analysis <- sort(sample.int(n, size = min(size, n), replace = FALSE))
        } else {
          units <- resolve_vector(unit, "unit")
          unique_units <- unique(units)
          if (length(unique_units) < 2L) {
            .abort("Grouped resampling requires at least two sampling units.")
          }
          size <- max(1L, floor(prop * length(unique_units)))
          selected <- sample(unique_units, min(size, length(unique_units)), FALSE)
          analysis <- which(units %in% selected)
        }
        make_split(sprintf("%s_%03d", method, i), analysis)
      })
    })
  }

  split_ids <- vapply(out, `[[`, character(1), "id")
  if (anyDuplicated(split_ids)) {
    .abort("Every resampling split must have a unique `id`.")
  }

  structure(
    list(
      method = method,
      splits = out,
      seed = as.integer(seed),
      n = n,
      sample_ids = as.character(data$metadata$id)
    ),
    class = "som_resamples"
  )
}

#' @export
print.som_resamples <- function(x, ...) {
  analysis_n <- vapply(x$splits, function(s) length(s$analysis), integer(1))
  assessment_n <- vapply(x$splits, function(s) length(s$assessment), integer(1))
  cat("<som_resamples>\n")
  cat("  method    :", x$method, "\n")
  cat("  splits    :", length(x$splits), "\n")
  cat("  analysis  :", paste(range(analysis_n), collapse = "-"), "rows\n")
  cat("  assessment:", paste(range(assessment_n), collapse = "-"), "rows\n")
  invisible(x)
}
