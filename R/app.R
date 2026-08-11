.parse_integer_set <- function(x, name, lower = 1L) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    .abort(sprintf("`%s` must be one comma-separated integer string.", name))
  }
  compact <- gsub("\\s+", "", x)
  tokens <- strsplit(compact, ",", fixed = TRUE)[[1L]]
  valid_tokens <- nzchar(compact) && length(tokens) > 0L &&
    all(grepl("^[0-9]+$", tokens))
  values <- if (valid_tokens) suppressWarnings(as.numeric(tokens)) else NA_real_
  if (!valid_tokens || anyNA(values) || any(!is.finite(values)) ||
        any(values < lower) || any(values > .Machine$integer.max)) {
    .abort(sprintf(
      "`%s` must be comma-separated integers of at least %d.", name, lower
    ))
  }
  unique(as.integer(values))
}

.gui_integer <- function(x, name, lower = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
        x != floor(x) || x < lower || x > .Machine$integer.max) {
    .abort(sprintf("`%s` must be one integer of at least %d.", name, lower))
  }
  as.integer(x)
}

.gui_metadata_columns <- function(config) {
  fields <- c(
    "id_column", "group_column", "time_column", "domain_column",
    "weight_column", "external_column"
  )
  unname(unlist(config[fields], use.names = FALSE))
}

.gui_metadata_candidates <- function() {
  list(
    id_column = "sample_id",
    group_column = c("sampling_group", "watershed_group"),
    time_column = "survey_date",
    domain_column = "reporting_region",
    weight_column = c("survey_weight", "sampling_weight", "sample_weight"),
    external_column = c("external_label", "class_label")
  )
}

.gui_canonical_metadata <- function() {
  c(
    id_column = "id",
    group_column = "group",
    time_column = "time",
    domain_column = "domain",
    weight_column = "weight",
    external_column = "external_label"
  )
}

.gui_metadata_defaults <- function(columns) {
  defaults <- vapply(.gui_metadata_candidates(), function(candidates) {
    present <- candidates[candidates %in% columns]
    if (length(present)) present[[1L]] else "None"
  }, character(1))

  canonical <- .gui_canonical_metadata()
  canonical_present <- canonical %in% columns
  if (sum(canonical_present) >= 2L) {
    use <- canonical_present & defaults[names(canonical)] == "None"
    defaults[names(canonical)[use]] <- canonical[use]
  }
  defaults
}

.gui_default_predictors <- function(raw, n = 9L) {
  numeric_columns <- names(raw)[vapply(raw, is.numeric, logical(1))]
  reserved_metadata <- unique(c(
    unlist(.gui_metadata_candidates(), use.names = FALSE),
    unname(.gui_canonical_metadata())
  ))
  utils::head(setdiff(numeric_columns, reserved_metadata), n)
}

.gui_predictor_defaults <- function(raw, data_source) {
  if (identical(data_source, "built_in")) {
    .gui_default_predictors(raw)
  } else {
    character()
  }
}

.gui_data_audit <- function(raw, predictors = character()) {
  if (!is.data.frame(raw) || !nrow(raw) || !ncol(raw)) {
    return(data.frame(
      Check = "Data source", Result = "No non-empty data frame is available",
      Status = "Not ready", check.names = FALSE
    ))
  }
  predictors <- intersect(predictors %||% character(), names(raw))
  numeric_columns <- names(raw)[vapply(raw, is.numeric, logical(1))]
  numeric_predictors <- intersect(predictors, numeric_columns)
  selected <- if (length(numeric_predictors)) {
    as.matrix(raw[numeric_predictors])
  } else {
    matrix(numeric(), nrow = nrow(raw), ncol = 0L)
  }
  non_finite <- if (length(selected)) {
    sum(!is.na(selected) & !is.finite(selected))
  } else {
    0L
  }
  incomplete_rows <- if (ncol(selected)) {
    sum(!stats::complete.cases(selected))
  } else {
    0L
  }
  empty_rows <- if (ncol(selected)) sum(rowSums(!is.na(selected)) == 0L) else 0L
  constant <- if (length(numeric_predictors)) {
    numeric_predictors[vapply(raw[numeric_predictors], function(x) {
      observed <- x[is.finite(x)]
      length(unique(observed)) <= 1L
    }, logical(1))]
  } else {
    character()
  }
  predictor_status <- if (length(predictors)) "Ready" else "Select predictors"
  missing_status <- if (incomplete_rows) "Review" else "Ready"
  finite_status <- if (non_finite) "Resolve" else "Ready"
  empty_status <- if (empty_rows) "Resolve" else "Ready"
  constant_status <- if (length(constant)) "Review" else "Ready"
  data.frame(
    Check = c(
      "Rows", "Columns", "Numeric columns", "Selected predictors",
      "Rows with missing predictor values", "Rows with no observed predictor",
      "Non-finite predictor values", "Constant predictors"
    ),
    Result = c(
      nrow(raw), ncol(raw), length(numeric_columns),
      if (length(predictors)) paste(predictors, collapse = ", ") else "None",
      incomplete_rows, empty_rows, non_finite,
      if (length(constant)) paste(constant, collapse = ", ") else "None"
    ),
    Status = c(
      "Ready", "Ready", "Ready", predictor_status, missing_status,
      empty_status, finite_status, constant_status
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.validate_gui_config <- function(config, raw) {
  if (!is.data.frame(raw) || !nrow(raw) || !ncol(raw)) {
    .abort("The selected data source must contain a non-empty data frame.")
  }
  if (is.null(names(raw)) || anyNA(names(raw)) || any(names(raw) == "") ||
        anyDuplicated(names(raw))) {
    .abort("The data source must have unique, non-empty column names.")
  }
  if (!length(config$predictors) || anyDuplicated(config$predictors)) {
    .abort("Select one or more unique numeric predictors.")
  }
  missing_predictors <- setdiff(config$predictors, names(raw))
  if (length(missing_predictors)) {
    .abort(sprintf(
      "Predictor columns were not found: %s.",
      paste(missing_predictors, collapse = ", ")
    ))
  }
  non_numeric <- config$predictors[
    !vapply(raw[config$predictors], is.numeric, logical(1))
  ]
  if (length(non_numeric)) {
    .abort(sprintf(
      "Predictors must be numeric: %s.",
      paste(non_numeric, collapse = ", ")
    ))
  }
  predictor_matrix <- as.matrix(raw[config$predictors])
  if (any(!is.na(predictor_matrix) & !is.finite(predictor_matrix))) {
    .abort("Predictors must not contain infinite values.")
  }
  if (any(rowSums(!is.na(predictor_matrix)) == 0L)) {
    .abort("Every row must contain at least one observed predictor value.")
  }

  metadata_columns <- .gui_metadata_columns(config)
  missing_metadata <- setdiff(metadata_columns, names(raw))
  if (length(missing_metadata)) {
    .abort(sprintf(
      "Metadata columns were not found: %s.",
      paste(missing_metadata, collapse = ", ")
    ))
  }
  overlap <- intersect(config$predictors, metadata_columns)
  if (length(overlap)) {
    .abort(sprintf(
      paste0(
        "Design metadata and external labels cannot enter the training ",
        "predictors: %s."
      ),
      paste(overlap, collapse = ", ")
    ))
  }
  if (!is.null(config$weight_column)) {
    weights <- raw[[config$weight_column]]
    if (!is.numeric(weights) || anyNA(weights) || any(!is.finite(weights)) ||
          any(weights < 0)) {
      .abort(paste0(
        "The selected weight column must be numeric, finite, non-missing ",
        "and non-negative."
      ))
    }
  }
  if (config$resample_method == "group_subsample" &&
        is.null(config$group_column)) {
    .abort("Grouped resampling requires a sampling-group column.")
  }
  if (config$resample_method == "leave_domain_out" &&
        is.null(config$domain_column)) {
    .abort("Leave-domain-out resampling requires a domain column.")
  }
  invisible(config)
}

.prepare_gui_analysis <- function(config, raw) {
  .validate_gui_config(config, raw)
  metadata_fields <- c(
    "id_column", "group_column", "time_column", "domain_column",
    "weight_column", "external_column"
  )
  values <- lapply(config[metadata_fields], function(column) {
    if (is.null(column)) NULL else raw[[column]]
  })
  data <- som_data(
    x = raw[, config$predictors, drop = FALSE],
    id = values$id_column,
    group = values$group_column,
    time = values$time_column,
    domain = values$domain_column,
    weight = values$weight_column,
    external_label = values$external_column
  )
  preprocessing <- som_preprocess(
    config$transform,
    center = config$center,
    scale = config$scale,
    zero_replacement = config$zero_replacement
  )
  # Validate transformation requirements before reporting a ready preflight,
  # including workflows that do not request cross-model references.
  invisible(.transform_matrix(data$layers$data, preprocessing))
  resample_arguments <- list(
    data = data,
    method = config$resample_method,
    seed = config$resample_seed
  )
  if (config$resample_method %in% c("subsample", "group_subsample")) {
    resample_arguments$repeats <- config$repeats
    resample_arguments$prop <- config$prop
  }
  if (config$resample_method == "group_subsample") {
    resample_arguments$unit <- "group"
  }
  if (config$resample_method == "leave_domain_out") {
    resample_arguments$domain <- "domain"
  }
  resamples <- do.call(som_resamples, resample_arguments)
  specification <- som_spec(
    c(config$xdim, config$ydim),
    seeds = config$seeds,
    rlen = config$rlen,
    k = config$k
  )

  split_analysis_n <- vapply(
    resamples$splits, function(split) length(split$analysis), integer(1)
  )
  grid_units <- specification$grids$xdim * specification$grids$ydim
  split_grid_feasible <- outer(split_analysis_n, grid_units, `>=`)
  feasible_som_fits <- as.integer(
    sum(split_grid_feasible) * length(specification$seeds)
  )
  model_budget <- length(resamples$splits) *
    nrow(expand_som_spec(specification))
  infeasible_som_fits <- as.integer(model_budget - feasible_som_fits)
  max_pairwise_comparisons <- .gui_integer(
    config$max_pairwise_comparisons %||% 1000000L,
    "max_pairwise_comparisons"
  )
  planned_pairwise_comparisons <- choose(as.double(model_budget), 2) *
    length(unique(specification$k))
  pairwise_budget_ok <- is.finite(planned_pairwise_comparisons) &&
    planned_pairwise_comparisons <= max_pairwise_comparisons

  analysis_keys <- vapply(resamples$splits, function(split) {
    paste(sort(unique(split$analysis)), collapse = ",")
  }, character(1))
  duplicate_analysis_splits <- as.integer(sum(duplicated(analysis_keys)))

  cross_model_feasible_splits <- NA_integer_
  if (length(config$cross_models)) {
    largest_k <- max(specification$k)
    cross_model_feasible_splits <- as.integer(sum(vapply(
      resamples$splits,
      function(split) {
        analysis <- split$analysis
        if (length(analysis) <= largest_k) return(FALSE)
        prepared <- tryCatch(
          .reference_matrix(
            data, analysis, preprocessing,
            specification$layer_weights,
            specification$normalize_layers
          ),
          error = function(e) e
        )
        !inherits(prepared, "error")
      },
      logical(1)
    )))
  }

  notes <- character()
  if (infeasible_som_fits > 0L && feasible_som_fits > 0L) {
    notes <- c(
      notes,
      sprintf(
        paste0(
          "%d of %d planned SOM fits do not meet the structural prerequisite ",
          "that analysis rows must be at least the number of map units."
        ),
        infeasible_som_fits, model_budget
      )
    )
  }
  if (duplicate_analysis_splits > 0L) {
    notes <- c(
      notes,
      sprintf(
        paste0(
          "%d resampling split%s repeat%s an earlier analysis set and ",
          "therefore do%s not add a distinct data perturbation."
        ),
        duplicate_analysis_splits,
        if (duplicate_analysis_splits == 1L) "" else "s",
        if (duplicate_analysis_splits == 1L) "s" else "",
        if (duplicate_analysis_splits == 1L) "es" else ""
      )
    )
  }
  if (length(config$cross_models) &&
        cross_model_feasible_splits < length(resamples$splits)) {
    notes <- c(
      notes,
      sprintf(
        paste0(
          "%d of %d resampling splits pass split-specific preprocessing and ",
          "have more analysis rows than the largest requested k (%d) for the ",
          "cross-model references. This is a computational eligibility check, ",
          "not a scientific assessment."
        ),
        cross_model_feasible_splits,
        length(resamples$splits),
        max(specification$k)
      )
    )
  }
  constant <- config$predictors[vapply(raw[config$predictors], function(x) {
    observed <- x[is.finite(x)]
    length(unique(observed)) <= 1L
  }, logical(1))]
  if (length(constant)) {
    notes <- c(
      notes,
      sprintf(
        "Constant predictors contribute no separation: %s.",
        paste(constant, collapse = ", ")
      )
    )
  }
  if ("gmm" %in% config$cross_models &&
        !requireNamespace("mclust", quietly = TRUE)) {
    notes <- c(notes, "GMM requires the suggested package `mclust`.")
  }
  list(
    data = data,
    preprocessing = preprocessing,
    resamples = resamples,
    specification = specification,
    model_budget = model_budget,
    planned_pairwise_comparisons = planned_pairwise_comparisons,
    max_pairwise_comparisons = max_pairwise_comparisons,
    pairwise_budget_ok = pairwise_budget_ok,
    feasible_som_fits = feasible_som_fits,
    infeasible_som_fits = infeasible_som_fits,
    duplicate_analysis_splits = duplicate_analysis_splits,
    cross_model_feasible_splits = cross_model_feasible_splits,
    notes = unique(notes)
  )
}

.gui_preflight_result <- function(config, raw) {
  tryCatch(
    {
      prepared <- .prepare_gui_analysis(config, raw)
      if (prepared$feasible_som_fits == 0L) {
        return(list(
          ok = FALSE,
          prepared = prepared,
          error = paste0(
            "No planned SOM fit meets the structural prerequisite that the ",
            "analysis split contain at least as many rows as map units. ",
            "Use a smaller grid or a design with more analysis rows."
          )
        ))
      }
      if (!prepared$pairwise_budget_ok) {
        return(list(
          ok = FALSE,
          prepared = prepared,
          error = paste0(
            "The planned workflow would create ",
            format(prepared$planned_pairwise_comparisons, trim = TRUE),
            " pairwise partition comparisons, exceeding the limit of ",
            format(prepared$max_pairwise_comparisons, trim = TRUE),
            ". Reduce seeds, grids, resampling splits or candidate k values."
          )
        ))
      }
      list(ok = TRUE, prepared = prepared)
    },
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
}

.format_gui_preflight_status <- function(result) {
  if (!isTRUE(result$ok)) {
    lines <- c("Preflight not ready.")
    if (!is.null(result$prepared)) {
      prepared <- result$prepared
      lines <- c(
        lines,
        sprintf(
          paste0(
            "SOM structure: %d planned fits; %d structurally feasible; ",
            "%d structurally ineligible."
          ),
          prepared$model_budget,
          prepared$feasible_som_fits,
          prepared$infeasible_som_fits
        ),
        sprintf(
          "Pairwise budget: %s planned; limit %s.",
          format(prepared$planned_pairwise_comparisons, trim = TRUE),
          format(prepared$max_pairwise_comparisons, trim = TRUE)
        )
      )
    }
    return(paste(c(lines, paste0("Resolve: ", result$error)), collapse = "\n"))
  }

  prepared <- result$prepared
  lines <- c(
    if (length(prepared$notes)) {
      "Preflight ready with review items."
    } else {
      "Preflight ready."
    },
    sprintf(
      "%d samples; %d predictors; %d resampling splits.",
      nrow(prepared$data$metadata),
      ncol(prepared$data$layers$data),
      length(prepared$resamples$splits)
    ),
    sprintf(
      paste0(
        "SOM structure: %d planned fits; %d structurally feasible; ",
        "%d structurally ineligible."
      ),
      prepared$model_budget,
      prepared$feasible_som_fits,
      prepared$infeasible_som_fits
    ),
    sprintf(
      "Pairwise budget: %s planned; limit %s.",
      format(prepared$planned_pairwise_comparisons, trim = TRUE),
      format(prepared$max_pairwise_comparisons, trim = TRUE)
    )
  )
  if (!is.na(prepared$cross_model_feasible_splits)) {
    lines <- c(
      lines,
      sprintf(
        paste0(
          "Cross-model prerequisites: %d/%d splits pass split-specific ",
          "preprocessing and have more rows than the largest requested k (%d)."
        ),
        prepared$cross_model_feasible_splits,
        length(prepared$resamples$splits),
        max(prepared$specification$k)
      )
    )
  }
  lines <- c(
    lines,
    sprintf(
      "Repeated analysis sets beyond the first: %d.",
      prepared$duplicate_analysis_splits
    )
  )
  if (length(prepared$notes)) {
    lines <- c(lines, paste0("Review: ", prepared$notes))
  }
  paste(lines, collapse = "\n")
}

.gui_workflow_diagnostics <- function(workflow) {
  issue_row <- function(stream, type, table, field) {
    count <- if (is.data.frame(table)) nrow(table) else 0L
    detail <- if (count && field %in% names(table)) {
      paste(
        utils::head(unique(as.character(table[[field]])), 2L),
        collapse = " | "
      )
    } else {
      "None"
    }
    data.frame(
      Stream = stream, Type = type, Count = count, Example = detail,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  rows <- list(
    issue_row("SOM ensemble", "Failures", workflow$ensemble$failures, "error"),
    issue_row(
      "SOM ensemble", "Warnings", workflow$ensemble$warnings, "warning"
    ),
    issue_row(
      "Consensus", "Failures", workflow$consensus_failures, "error"
    )
  )
  if (!is.null(workflow$cross_models)) {
    rows <- c(rows, list(
      issue_row(
        "Cross-model references", "Failures",
        workflow$cross_models$failures, "error"
      ),
      issue_row(
        "Cross-model references", "Warnings",
        workflow$cross_models$warnings, "warning"
      )
    ))
  }
  do.call(rbind, rows)
}

.gui_workflow_status <- function(workflow) {
  successful_som <- sum(vapply(
    workflow$ensemble$fits, function(x) isTRUE(x$success), logical(1)
  ))
  lines <- c(
    sprintf(
      "SOM fits: %d/%d succeeded; %d failed; %s.",
      successful_som, workflow$ensemble$expected_models,
      nrow(workflow$ensemble$failures),
      .count_noun(nrow(workflow$ensemble$warnings), "warning")
    ),
    sprintf(
      "Consensus: %d computed; %d not computed.",
      length(workflow$consensus), nrow(workflow$consensus_failures)
    )
  )
  if (!is.null(workflow$cross_models)) {
    lines <- c(lines, sprintf(
      "Cross-model fits: %d succeeded; %d failed; %s.",
      length(workflow$cross_models$records),
      nrow(workflow$cross_models$failures),
      .count_noun(nrow(workflow$cross_models$warnings), "warning")
    ))
  } else {
    lines <- c(lines, "Cross-model fits: not requested.")
  }
  paste(lines, collapse = "\n")
}

.gui_consensus_choices <- function(workflow) {
  keys <- names(workflow$consensus)
  if (!length(keys)) return(character())
  stats::setNames(keys, paste0("k = ", sub("^k", "", keys)))
}

.gui_table_labels <- function(x) {
  if (!is.data.frame(x)) return(x)
  labels <- c(
    k = "k", scope = "Evidence scope", n_pairs = "Pairs",
    n_pairs_evaluable = "Evaluable pairs", median_joint_n = "Median joint n",
    median_joint_coverage = "Median joint coverage",
    median_ari = "Median ARI", ari_q025 = "ARI 2.5%",
    ari_q975 = "ARI 97.5%", median_ami = "Median AMI",
    ami_q025 = "AMI 2.5%", ami_q975 = "AMI 97.5%",
    method = "Reference method", n_comparisons = "Comparisons"
  )
  original <- names(x)
  fallback <- tools::toTitleCase(gsub("_", " ", original, fixed = TRUE))
  replacement <- unname(labels[original])
  names(x) <- ifelse(is.na(replacement), fallback, replacement)
  rownames(x) <- NULL
  x
}

.code_value <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")

.gui_configuration_snapshot <- function(config) {
  reserved <- c("snapshot_type", "snapshot_note")
  config[reserved] <- NULL
  c(
    list(
      snapshot_type = "SOMevidence GUI configuration snapshot",
      snapshot_note = paste0(
        "This snapshot documents the settings used for a run. ",
        "SOMevidence 1.1.x cannot import it to restore GUI controls; ",
        "use the exported R script as the executable analysis record."
      )
    ),
    config
  )
}

.render_gui_script <- function(config) {
  metadata_expression <- function(column) {
    if (is.null(column)) "NULL" else sprintf("raw[[%s]]", .code_value(column))
  }
  source_lines <- if (config$data_source == "built_in") {
    c(
      paste0(
        "simulated <- simulate_som_scenario(\"clusters\", ",
        "n = 180, p = 6, seed = 1)"
      ),
      "raw <- as.data.frame(simulated$layers$environment)",
      "raw$sample_id <- simulated$metadata$id",
      "raw$external_label <- simulated$metadata$external_label"
    )
  } else {
    input_file <- config$input_file %||% "replace-with-your-data.csv"
    input_file <- basename(gsub("\\\\", "/", input_file))
    c(
      "# Copy the input CSV into the project data directory before running.",
      sprintf(
        "input_path <- file.path(\"data\", %s)",
        .code_value(input_file)
      ),
      "if (!file.exists(input_path)) {",
      paste0(
        "  stop(\"Input CSV not found at project-relative path: \", ",
        "input_path)"
      ),
      "}",
      "input_lines <- readLines(input_path, warn = FALSE)",
      "raw <- utils::read.csv(text = input_lines, check.names = FALSE)"
    )
  }
  resample_arguments <- c(
    "data",
    sprintf("method = %s", .code_value(config$resample_method)),
    sprintf("seed = %dL", config$resample_seed)
  )
  if (config$resample_method %in% c("subsample", "group_subsample")) {
    resample_arguments <- c(
      resample_arguments,
      sprintf("repeats = %dL", config$repeats),
      sprintf("prop = %s", .code_value(config$prop))
    )
  }
  if (config$resample_method == "group_subsample") {
    resample_arguments <- c(resample_arguments, "unit = \"group\"")
  }
  if (config$resample_method == "leave_domain_out") {
    resample_arguments <- c(resample_arguments, "domain = \"domain\"")
  }
  package_version <- config$package_version %||%
    as.character(utils::packageVersion("SOMevidence"))
  max_pairwise_comparisons <- .gui_integer(
    config$max_pairwise_comparisons %||% 1000000L,
    "max_pairwise_comparisons"
  )

  c(
    sprintf(
      "# GUI configuration snapshot schema: %d",
      config$schema_version %||% 1L
    ),
    paste0(
      "# The 1.1.x GUI cannot import this snapshot to restore controls; ",
      "this script is the executable record."
    ),
    sprintf("# Generated with SOMevidence %s", package_version),
    "library(SOMevidence)",
    sprintf(
      "required_SOMevidence_version <- %s",
      .code_value(package_version)
    ),
    paste0(
      "if (!identical(as.character(utils::packageVersion(\"SOMevidence\")), ",
      paste0(
        "required_SOMevidence_version)) warning(\"The installed ",
        "SOMevidence version "
      ),
      "differs from the version used to generate this script.\")"
    ),
    "",
    source_lines,
    "",
    "data <- som_data(",
    sprintf("  x = raw[, %s, drop = FALSE],", .code_value(config$predictors)),
    sprintf("  id = %s,", metadata_expression(config$id_column)),
    sprintf("  group = %s,", metadata_expression(config$group_column)),
    sprintf("  time = %s,", metadata_expression(config$time_column)),
    sprintf("  domain = %s,", metadata_expression(config$domain_column)),
    sprintf("  weight = %s,", metadata_expression(config$weight_column)),
    sprintf(
      "  external_label = %s",
      metadata_expression(config$external_column)
    ),
    ")",
    sprintf(
      paste0(
        "preprocessing <- som_preprocess(transform = %s, center = %s, ",
        "scale = %s, zero_replacement = %s)"
      ),
      .code_value(config$transform),
      .code_value(config$center),
      .code_value(config$scale),
      .code_value(config$zero_replacement)
    ),
    sprintf(
      "resamples <- som_resamples(%s)",
      paste(resample_arguments, collapse = ", ")
    ),
    "specification <- som_spec(",
    sprintf("  grids = c(%dL, %dL),", config$xdim, config$ydim),
    sprintf("  seeds = %s,", .code_value(config$seeds)),
    sprintf("  rlen = %dL,", config$rlen),
    sprintf("  k = %s", .code_value(config$k)),
    ")",
    "workflow <- run_som_workflow(",
    "  data, specification, resamples,",
    "  preprocess = preprocessing,",
    sprintf("  cross_models = %s,", .code_value(config$cross_models)),
    sprintf(
      "  max_pairwise_comparisons = %dL",
      max_pairwise_comparisons
    ),
    ")",
    "workflow"
  )
}

.read_gui_csv <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .abort("Select one CSV file before continuing.")
  }
  read_error <- function() {
    .abort(paste0(
      "The CSV could not be read. Confirm that it is a comma-separated ",
      "text file with a header row, consistent columns, and a supported ",
      "text encoding."
    ))
  }
  tryCatch(
    {
      lines <- withCallingHandlers(
        readLines(path, warn = FALSE),
        warning = function(w) read_error()
      )
      withCallingHandlers(
        utils::read.csv(text = lines, check.names = FALSE),
        warning = function(w) read_error()
      )
    },
    error = function(e) read_error()
  )
}

#' Launch the optional reproducible SOM interface
#'
#' The Shiny interface exposes a compact subset of the package workflow for
#' teaching and exploratory configuration. Every completed run can export its
#' exact R script and a YAML configuration snapshot. Version 1.1.x cannot
#' import that snapshot to restore GUI controls. The exported script, not the
#' GUI session or snapshot, is the executable analysis record. The interface is
#' designed for a local R session, and `SOMevidence` sends no telemetry. In a
#' local session, selected files remain on the local computer. A remotely
#' deployed Shiny application transfers selected files to its host, whose
#' operator is responsible for access controls and data handling.
#'
#' @section Lifecycle:
#' Experimental. The interface and its exported configuration snapshot schema
#' may change after independent usability testing. The ordinary R API remains
#' the authoritative analysis interface.
#'
#' @return A `shiny.appobj`. Pass it to [shiny::runApp()] or print it in an
#'   interactive R session. The application does not return an analysis result;
#'   users can export the executable R script for a configured run.
#' @export
launch_som_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    .abort("Install the suggested package `shiny` to launch the interface.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .abort("Install the suggested package `ggplot2` to launch the interface.")
  }

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(
      ".somevidence-table-scroll { overflow-x: auto; width: 100%; } ",
      ".somevidence-table-scroll table { white-space: nowrap; } ",
      ".somevidence-footer { color: #666; font-size: 0.9em; margin-top: 1em; }"
    )))),
    shiny::titlePanel("SOM evidence workflow"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          "data_source", "Data source",
          choices = c(
            "Built-in simulation" = "built_in", "Upload CSV" = "upload"
          )
        ),
        shiny::conditionalPanel(
          "input.data_source == 'upload'",
          shiny::fileInput("file", "CSV file", accept = ".csv"),
          shiny::helpText(
            "For uploaded data, predictors are intentionally not selected ",
            "automatically. Review numeric identifiers, coordinates, dates ",
            "and design variables before choosing the training features."
          )
        ),
        shiny::selectizeInput(
          "predictors", "Numeric predictors", choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput("id_column", "Sample ID", choices = "None"),
        shiny::selectInput("group_column", "Sampling group", choices = "None"),
        shiny::selectInput("time_column", "Sampling time", choices = "None"),
        shiny::selectInput(
          "domain_column", "Transfer domain", choices = "None"
        ),
        shiny::selectInput(
          "weight_column", "Survey/summary weight (metadata only)",
          choices = "None"
        ),
        shiny::selectInput(
          "external_column", "External label", choices = "None"
        ),
        shiny::helpText(
          "Selected metadata and external-label columns are retained outside ",
          "the SOM training matrix. Weight is not used as a training weight."
        ),
        shiny::selectInput(
          "transform", "Transformation",
          choices = c("identity", "log", "log1p", "sqrt", "hellinger", "clr")
        ),
        shiny::checkboxInput("center", "Center variables", TRUE),
        shiny::checkboxInput("scale", "Scale variables", TRUE),
        shiny::conditionalPanel(
          "input.transform == 'clr'",
          shiny::numericInput(
            "zero_replacement", "CLR zero replacement", 1e-6,
            min = .Machine$double.eps
          )
        ),
        shiny::selectInput(
          "resample_method", "Resampling",
          choices = c(
            "full", "subsample", "group_subsample", "leave_domain_out"
          )
        ),
        shiny::conditionalPanel(
          paste0(
            "input.resample_method == 'subsample' || ",
            "input.resample_method == 'group_subsample'"
          ),
          shiny::numericInput("repeats", "Resample repeats", 5L, min = 1L),
          shiny::numericInput(
            "prop", "Analysis proportion", 0.8, min = 0.1, max = 1
          )
        ),
        shiny::numericInput("xdim", "Grid width", 7L, min = 2L),
        shiny::numericInput("ydim", "Grid height", 5L, min = 2L),
        shiny::textInput("seeds", "SOM seeds", "1,2,3"),
        shiny::numericInput("rlen", "Training iterations", 500L, min = 10L),
        shiny::textInput("k", "Candidate k", "2,3,4,5"),
        shiny::checkboxGroupInput(
          "cross_models", "Cross-model references",
          choices = c("K-means" = "kmeans", "Ward.D2" = "ward", "GMM" = "gmm"),
          selected = c("kmeans", "ward")
        ),
        shiny::actionButton("run", "Run workflow"),
        shiny::downloadButton("download_r", "Export R script"),
        shiny::downloadButton(
          "download_yaml", "Export configuration snapshot"
        ),
        shiny::helpText(
          "The R script is the executable record. The YAML configuration ",
          "snapshot documents the selected settings; SOMevidence 1.1.x ",
          "cannot import it to restore GUI controls."
        )
      ),
      shiny::mainPanel(
        shiny::h4("Data preflight"),
        shiny::verbatimTextOutput("preflight_status"),
        shiny::div(
          class = "somevidence-table-scroll",
          shiny::tableOutput("data_audit")
        ),
        shiny::hr(),
        shiny::radioButtons(
          "plot_type", "Evidence view",
          choices = c(
            "Representation" = "audit",
            "Partition stability" = "partitions",
            "Consensus" = "consensus",
            "Cross-model agreement" = "cross_model"
          )
        ),
        shiny::conditionalPanel(
          "input.plot_type == 'consensus'",
          shiny::selectInput(
            "consensus_k", "Consensus solution", choices = character()
          )
        ),
        shiny::verbatimTextOutput("status"),
        shiny::plotOutput("evidence_plot", height = "520px"),
        shiny::h4("Run diagnostics"),
        shiny::div(
          class = "somevidence-table-scroll",
          shiny::tableOutput("diagnostics_table")
        ),
        shiny::h4("Partition stability"),
        shiny::div(
          class = "somevidence-table-scroll",
          shiny::tableOutput("partition_table")
        ),
        shiny::h4("Cross-model agreement"),
        shiny::div(
          class = "somevidence-table-scroll",
          shiny::tableOutput("cross_table")
        )
      )
    ),
    shiny::tags$footer(
      class = "somevidence-footer",
      shiny::tags$hr(),
      shiny::tags$p(
        paste0(
          "Experimental interface | SOMevidence ",
          as.character(utils::packageVersion("SOMevidence")),
          " | Designed for a local R session | No telemetry is sent by ",
          "SOMevidence."
        )
      )
    )
  )

  server <- function(input, output, session) {
    built_in <- function() {
      simulated <- simulate_som_scenario("clusters", n = 180, p = 6, seed = 1)
      raw <- as.data.frame(simulated$layers$environment)
      raw$sample_id <- simulated$metadata$id
      raw$external_label <- simulated$metadata$external_label
      raw
    }
    raw_data <- shiny::reactive({
      if (input$data_source == "built_in") return(built_in())
      shiny::req(input$file)
      .read_gui_csv(input$file$datapath)
    })
    shiny::observeEvent(raw_data(), {
      raw <- raw_data()
      numeric_columns <- names(raw)[vapply(raw, is.numeric, logical(1))]
      metadata_defaults <- .gui_metadata_defaults(names(raw))
      default_predictors <- .gui_predictor_defaults(raw, input$data_source)
      metadata_choices <- c("None", names(raw))
      shiny::updateSelectizeInput(
        session, "predictors",
        choices = numeric_columns, selected = default_predictors,
        server = TRUE
      )
      shiny::updateSelectInput(
        session, "id_column",
        choices = metadata_choices,
        selected = metadata_defaults[["id_column"]]
      )
      for (id in setdiff(names(metadata_defaults), "id_column")) {
        shiny::updateSelectInput(
          session, id,
          choices = metadata_choices, selected = metadata_defaults[[id]]
        )
      }
    }, ignoreInit = FALSE)

    as_column <- function(value) if (identical(value, "None")) NULL else value
    gui_config <- shiny::reactive({
      list(
        schema_version = 1L,
        package_version = as.character(utils::packageVersion("SOMevidence")),
        data_source = input$data_source,
        input_file = if (input$data_source == "upload") {
          input$file$name %||% "replace-with-your-data.csv"
        } else {
          NULL
        },
        predictors = input$predictors,
        id_column = as_column(input$id_column),
        group_column = as_column(input$group_column),
        time_column = as_column(input$time_column),
        domain_column = as_column(input$domain_column),
        weight_column = as_column(input$weight_column),
        external_column = as_column(input$external_column),
        transform = input$transform,
        center = input$center,
        scale = input$scale,
        zero_replacement = if (input$transform == "clr") {
          input$zero_replacement
        } else {
          NULL
        },
        resample_method = input$resample_method,
        repeats = .gui_integer(input$repeats %||% 5L, "repeats"),
        prop = input$prop %||% 0.8,
        resample_seed = 1L,
        xdim = .gui_integer(input$xdim, "xdim", lower = 2L),
        ydim = .gui_integer(input$ydim, "ydim", lower = 2L),
        seeds = .parse_integer_set(input$seeds, "seeds"),
        rlen = .gui_integer(input$rlen, "rlen", lower = 10L),
        k = .parse_integer_set(input$k, "k", lower = 2L),
        cross_models = input$cross_models %||% character(),
        max_pairwise_comparisons = 1000000L
      )
    })
    preflight <- shiny::reactive({
      raw <- raw_data()
      if (!length(input$predictors)) {
        return(list(
          ok = FALSE,
          error = paste0(
            "Select predictors after reviewing the numeric columns and design ",
            "metadata. No uploaded predictors are selected automatically."
          )
        ))
      }
      .gui_preflight_result(gui_config(), raw)
    })

    output$data_audit <- shiny::renderTable({
      .gui_data_audit(raw_data(), input$predictors)
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$preflight_status <- shiny::renderText({
      .format_gui_preflight_status(preflight())
    })

    analysis <- shiny::eventReactive(input$run, {
      result <- preflight()
      shiny::validate(shiny::need(result$ok, result$error))
      prepared <- result$prepared
      config <- gui_config()
      workflow <- shiny::withProgress(
        message = "Fitting ensemble", value = 0.3,
        {
          run_som_workflow(
            prepared$data,
            prepared$specification,
            prepared$resamples,
            preprocess = prepared$preprocessing,
            cross_models = config$cross_models,
            max_pairwise_comparisons = prepared$max_pairwise_comparisons,
            keep_models = FALSE
          )
        }
      )
      list(workflow = workflow, config = config, preflight = prepared)
    })

    shiny::observeEvent(analysis(), {
      workflow <- analysis()$workflow
      labels <- .gui_consensus_choices(workflow)
      keys <- unname(labels)
      current <- input$consensus_k
      selected <- if (!is.null(current) && current %in% keys) {
        current
      } else if (length(keys)) {
        keys[[1L]]
      } else {
        character()
      }
      shiny::updateSelectInput(
        session, "consensus_k", choices = labels, selected = selected
      )
    })

    output$status <- shiny::renderText({
      .gui_workflow_status(analysis()$workflow)
    })
    output$evidence_plot <- shiny::renderPlot({
      workflow <- analysis()$workflow
      if (input$plot_type == "audit") return(plot(workflow$audit))
      if (input$plot_type == "partitions") return(plot(workflow$partitions))
      if (input$plot_type == "cross_model") {
        shiny::validate(shiny::need(
          !is.null(workflow$cross_comparison),
          "No cross-model result is available."
        ))
        return(plot(workflow$cross_comparison))
      }
      shiny::validate(shiny::need(
        length(workflow$consensus) > 0L,
        "No consensus result is available."
      ))
      consensus_key <- input$consensus_k
      if (is.null(consensus_key) ||
            !consensus_key %in% names(workflow$consensus)) {
        consensus_key <- names(workflow$consensus)[[1L]]
      }
      plot(workflow$consensus[[consensus_key]])
    })
    output$diagnostics_table <- shiny::renderTable({
      .gui_workflow_diagnostics(analysis()$workflow)
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$partition_table <- shiny::renderTable({
      .gui_table_labels(analysis()$workflow$partitions$stability)
    }, digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$cross_table <- shiny::renderTable({
      comparison <- analysis()$workflow$cross_comparison
      if (is.null(comparison)) {
        return(data.frame(Note = "Not requested", check.names = FALSE))
      }
      .gui_table_labels(comparison$summary)
    }, digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$download_r <- shiny::downloadHandler(
      filename = function() "som_workflow.R",
      content = function(file) {
        writeLines(.render_gui_script(analysis()$config), file)
      }
    )
    output$download_yaml <- shiny::downloadHandler(
      filename = function() "som_workflow_configuration_snapshot.yml",
      content = function(file) {
        if (!requireNamespace("yaml", quietly = TRUE)) {
          .abort("Install `yaml` to export the configuration snapshot.")
        }
        yaml::write_yaml(
          .gui_configuration_snapshot(analysis()$config),
          file
        )
      }
    )
  }
  shiny::shinyApp(ui, server)
}
