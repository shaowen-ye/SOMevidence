test_that("GUI export is parseable and records the analysis choices", {
  config <- list(
    data_source = "upload",
    input_file = "monitoring.csv",
    predictors = c("temperature", "nutrient"),
    id_column = "sample_id",
    group_column = "site",
    time_column = "date",
    domain_column = NULL,
    weight_column = "survey_weight",
    external_column = NULL,
    transform = "log1p",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "group_subsample",
    repeats = 10L,
    prop = 0.8,
    resample_seed = 1L,
    xdim = 7L,
    ydim = 5L,
    seeds = 1:3,
    rlen = 500L,
    k = 2:4,
    cross_models = c("kmeans", "ward"),
    max_pairwise_comparisons = 1000000L
  )
  script <- SOMevidence:::.render_gui_script(config)

  expect_error(parse(text = script), NA)
  expect_true(any(grepl("group_subsample", script, fixed = TRUE)))
  expect_true(any(grepl("monitoring.csv", script, fixed = TRUE)))
  expect_true(any(grepl("file.path(\"data\"", script, fixed = TRUE)))
  expect_true(any(grepl("file.exists(input_path)", script, fixed = TRUE)))
  expect_true(any(grepl(
    "readLines(input_path, warn = FALSE)", script, fixed = TRUE
  )))
  expect_true(any(grepl(
    "read.csv(text = input_lines", script, fixed = TRUE
  )))
  expect_false(any(grepl("read.csv(input_path", script, fixed = TRUE)))
  expect_true(any(grepl("cross_models", script, fixed = TRUE)))
  expect_true(any(grepl(
    "max_pairwise_comparisons = 1000000L", script, fixed = TRUE
  )))
  expect_true(any(grepl("time = raw[[\"date\"]]", script, fixed = TRUE)))
  expect_true(any(grepl(
    "weight = raw[[\"survey_weight\"]]", script, fixed = TRUE
  )))
  expect_true(any(grepl("required_SOMevidence_version", script, fixed = TRUE)))
  expect_true(any(grepl("configuration snapshot", script, fixed = TRUE)))
  expect_true(any(grepl(
    "cannot currently import this snapshot", script, fixed = TRUE
  )))

  snapshot <- SOMevidence:::.gui_configuration_snapshot(config)
  expect_identical(
    snapshot$snapshot_type,
    "SOMevidence GUI configuration snapshot"
  )
  expect_match(snapshot$snapshot_note, "cannot currently import")
  expect_identical(snapshot$predictors, config$predictors)
  expect_identical(
    snapshot$max_pairwise_comparisons,
    config$max_pairwise_comparisons
  )

  full_config <- config
  full_config$resample_method <- "full"
  full_script <- SOMevidence:::.render_gui_script(full_config)
  resample_line <- full_script[
    grepl("som_resamples", full_script, fixed = TRUE)
  ]
  expect_false(grepl("repeats", resample_line, fixed = TRUE))
  expect_false(grepl("prop", resample_line, fixed = TRUE))
})

test_that("GUI export reads an incomplete final CSV line under warn = 2", {
  project <- tempfile("somevidence-export-")
  dir.create(file.path(project, "data"), recursive = TRUE)
  csv <- file.path(project, "data", "monitoring.csv")
  rows <- data.frame(
    sample_id = sprintf("s%02d", 1:20),
    x = seq_len(20),
    y = seq_len(20) / 10
  )
  csv_text <- paste(
    c(
      paste(names(rows), collapse = ","),
      apply(rows, 1L, paste, collapse = ",")
    ),
    collapse = "\n"
  )
  writeBin(charToRaw(csv_text), csv)

  config <- list(
    data_source = "upload",
    input_file = csv,
    predictors = c("x", "y"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = NULL,
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "full",
    repeats = 1L,
    prop = 1,
    resample_seed = 1L,
    xdim = 2L,
    ydim = 2L,
    seeds = 1L,
    rlen = 10L,
    k = 2L,
    cross_models = character(),
    max_pairwise_comparisons = 1000000L,
    package_version = as.character(utils::packageVersion("SOMevidence"))
  )

  script <- SOMevidence:::.render_gui_script(config)
  expect_false(any(grepl(project, script, fixed = TRUE)))
  withr::local_dir(project)
  withr::local_options(list(warn = 2))
  result <- eval(parse(text = script), envir = new.env(parent = globalenv()))
  expect_s3_class(result, "som_workflow")
  expect_equal(nrow(result$ensemble$data$metadata), 20L)
})

test_that("GUI integer inputs reject coercion and truncation", {
  expect_identical(SOMevidence:::.parse_integer_set("2, 3,3", "k", 2L), 2:3)
  expect_error(SOMevidence:::.parse_integer_set("2.5,3", "k", 2L), "integers")
  expect_error(SOMevidence:::.parse_integer_set("2e0,3", "k", 2L), "integers")
  expect_error(SOMevidence:::.parse_integer_set("2,,3", "k", 2L), "integers")
  expect_identical(SOMevidence:::.gui_integer(10, "rlen", 10L), 10L)
  expect_error(SOMevidence:::.gui_integer(10.5, "rlen", 10L), "integer")
})

test_that("GUI configuration keeps metadata outside training predictors", {
  raw <- data.frame(
    sample_id = c("a", "b", "c"),
    site = c("x", "y", "z"),
    value = 1:3,
    survey_weight = c(1, 2, 1),
    external = c("A", "B", "A")
  )
  config <- list(
    predictors = "value",
    id_column = "sample_id",
    group_column = "site",
    time_column = NULL,
    domain_column = NULL,
    weight_column = "survey_weight",
    external_column = "external",
    resample_method = "group_subsample"
  )
  expect_invisible(SOMevidence:::.validate_gui_config(config, raw))

  config$predictors <- c("value", "survey_weight")
  expect_error(
    SOMevidence:::.validate_gui_config(config, raw),
    "cannot enter the training predictors"
  )
  config$predictors <- "value"
  raw$survey_weight[[2L]] <- -1
  expect_error(
    SOMevidence:::.validate_gui_config(config, raw),
    "non-negative"
  )

  monitoring_defaults <- SOMevidence:::.gui_metadata_defaults(c(
    "sample_id", "watershed_group", "survey_date", "reporting_region",
    "survey_weight", "measure__temperature"
  ))
  expect_identical(
    unname(monitoring_defaults[c(
      "id_column", "group_column", "time_column", "domain_column",
      "weight_column"
    )]),
    c(
      "sample_id", "watershed_group", "survey_date", "reporting_region",
      "survey_weight"
    )
  )
  expect_true("survey_weight" %in% unlist(
    SOMevidence:::.gui_metadata_candidates(), use.names = FALSE
  ))
  expect_false(any(c("weight", "class", "time") %in% unlist(
    SOMevidence:::.gui_metadata_candidates(), use.names = FALSE
  )))

  canonical_defaults <- SOMevidence:::.gui_metadata_defaults(c(
    "id", "group", "time", "domain", "weight", "external_label",
    "temperature"
  ))
  expect_identical(
    unname(canonical_defaults),
    c("id", "group", "time", "domain", "weight", "external_label")
  )

  ambiguous_defaults <- SOMevidence:::.gui_metadata_defaults(c(
    "sample", "weight", "temperature"
  ))
  expect_identical(ambiguous_defaults[["weight_column"]], "None")
  expect_identical(
    SOMevidence:::.gui_default_predictors(data.frame(
      sample = letters[1:3], weight = 1:3, temperature = 4:6
    )),
    "temperature"
  )

  predictor_data <- data.frame(sample_id = letters[1:3], x = 1:3, y = 4:6)
  expect_identical(
    SOMevidence:::.gui_predictor_defaults(predictor_data, "built_in"),
    c("x", "y")
  )
  expect_identical(
    SOMevidence:::.gui_predictor_defaults(predictor_data, "upload"),
    character()
  )
})

test_that("GUI data audit and preflight expose risky inputs before fitting", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    site = rep(letters[1:3], each = 4),
    x = c(NA, seq_len(11)),
    y = rep(2, 12),
    z = seq(0.1, 1.2, length.out = 12)
  )
  audit <- SOMevidence:::.gui_data_audit(raw, c("x", "y", "z"))
  missing_status <- audit$Status[
    audit$Check == "Rows with missing predictor values"
  ]
  expect_identical(
    missing_status,
    "Not ready"
  )
  expect_identical(audit$Result[audit$Check == "Constant predictors"], "y")

  config <- list(
    predictors = c("x", "y", "z"),
    id_column = "sample_id",
    group_column = "site",
    time_column = NULL,
    domain_column = NULL,
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "group_subsample",
    repeats = 2L,
    prop = 0.67,
    resample_seed = 1L,
    xdim = 3L,
    ydim = 2L,
    seeds = 1:2,
    rlen = 10L,
    k = 2L,
    cross_models = "ward"
  )
  expect_error(
    SOMevidence:::.prepare_gui_analysis(config, raw),
    "must be complete"
  )
  complete_raw <- raw
  complete_raw$x[[1L]] <- 0
  prepared <- SOMevidence:::.prepare_gui_analysis(config, complete_raw)
  expect_s3_class(prepared$data, "som_data")
  expect_s3_class(prepared$resamples, "som_resamples")
  expect_equal(prepared$model_budget, 4L)
  expect_identical(prepared$feasible_som_fits, 4L)
  expect_identical(prepared$infeasible_som_fits, 0L)
  expect_identical(prepared$preprocessing_feasible_splits, 2L)
  expect_equal(nrow(prepared$preprocessing_failures), 0L)
  expect_type(prepared$duplicate_analysis_splits, "integer")
  expect_type(prepared$cross_model_feasible_splits, "integer")
  expect_match(paste(prepared$notes, collapse = " "), "Constant predictors")

  complete_raw$x[[2L]] <- Inf
  expect_error(
    SOMevidence:::.prepare_gui_analysis(config, complete_raw),
    "infinite"
  )
})

test_that("GUI preflight distinguishes planned and eligible fits", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    domain = c(rep("a", 2), rep("b", 4), rep("c", 6)),
    x = seq_len(12),
    y = seq_len(12) / 10
  )
  config <- list(
    predictors = c("x", "y"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = "domain",
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "leave_domain_out",
    repeats = 2L,
    prop = 0.8,
    resample_seed = 1L,
    xdim = 3L,
    ydim = 3L,
    seeds = 1:2,
    rlen = 10L,
    k = 2L,
    cross_models = "ward"
  )

  prepared <- SOMevidence:::.prepare_gui_analysis(config, raw)
  expect_equal(prepared$model_budget, 6L)
  expect_identical(prepared$feasible_som_fits, 2L)
  expect_identical(prepared$infeasible_som_fits, 4L)
  expect_identical(prepared$duplicate_analysis_splits, 0L)
  expect_identical(prepared$cross_model_feasible_splits, 3L)
  expect_match(paste(prepared$notes, collapse = " "), "planned SOM fits")
  ready_status <- SOMevidence:::.format_gui_preflight_status(list(
    ok = TRUE, prepared = prepared
  ))
  expect_match(ready_status, "Preflight ready with review items")
  expect_match(ready_status, "2 eligible")
  expect_match(ready_status, "3/3 splits")

  blocked_config <- config
  blocked_config$xdim <- 4L
  blocked_config$ydim <- 3L
  blocked <- SOMevidence:::.gui_preflight_result(blocked_config, raw)
  expect_false(blocked$ok)
  expect_identical(blocked$prepared$feasible_som_fits, 0L)
  expect_identical(blocked$prepared$infeasible_som_fits, 6L)
  expect_match(blocked$error, "No planned SOM fit")
  blocked_status <- SOMevidence:::.format_gui_preflight_status(blocked)
  expect_match(blocked_status, "Preflight not ready")
  expect_match(blocked_status, "0 eligible")
})

test_that("GUI preflight identifies repeated analysis sets", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    x = seq_len(12),
    y = seq_len(12) / 10
  )
  config <- list(
    predictors = c("x", "y"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = NULL,
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "subsample",
    repeats = 3L,
    prop = 1,
    resample_seed = 1L,
    xdim = 2L,
    ydim = 2L,
    seeds = 1L,
    rlen = 10L,
    k = 2L,
    cross_models = character()
  )

  prepared <- SOMevidence:::.prepare_gui_analysis(config, raw)
  expect_identical(prepared$feasible_som_fits, 3L)
  expect_identical(prepared$infeasible_som_fits, 0L)
  expect_identical(prepared$duplicate_analysis_splits, 2L)
  expect_identical(prepared$cross_model_feasible_splits, NA_integer_)
  expect_match(paste(prepared$notes, collapse = " "), "repeat")
  blocked <- SOMevidence:::.gui_preflight_result(config, raw)
  expect_false(blocked$ok)
  expect_match(blocked$error, "overweight the same data perturbation")
})

test_that("GUI preflight blocks missing predictors before SOM fitting", {
  raw <- data.frame(
    sample_id = paste0("s", 1:6),
    x1 = 1:6,
    x2 = c(2, 3, NA, 5, 6, 7)
  )
  config <- list(
    predictors = c("x1", "x2"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = NULL,
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "full",
    repeats = 1L,
    prop = 1,
    resample_seed = 1L,
    xdim = 2L,
    ydim = 2L,
    seeds = 1L,
    rlen = 10L,
    k = 2L,
    cross_models = character()
  )

  result <- SOMevidence:::.gui_preflight_result(config, raw)
  expect_false(result$ok)
  expect_match(result$error, "must be complete")
  expect_match(
    SOMevidence:::.format_gui_preflight_status(result),
    "Preflight not ready"
  )
})

test_that("GUI preflight validates transforms without cross-models", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    x = c(-1, seq_len(11)),
    y = seq_len(12) / 10
  )
  config <- list(
    predictors = c("x", "y"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = NULL,
    weight_column = NULL,
    external_column = NULL,
    transform = "log1p",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "full",
    repeats = 1L,
    prop = 1,
    resample_seed = 1L,
    xdim = 2L,
    ydim = 2L,
    seeds = 1L,
    rlen = 10L,
    k = 2L,
    cross_models = character()
  )

  invalid <- SOMevidence:::.gui_preflight_result(config, raw)
  expect_false(invalid$ok)
  expect_match(invalid$error, "requires non-negative values")
  expect_match(
    SOMevidence:::.format_gui_preflight_status(invalid),
    "Preflight not ready"
  )

  config$transform <- "identity"
  valid <- SOMevidence:::.gui_preflight_result(config, raw)
  expect_true(valid$ok)
})

test_that("GUI preflight blocks workflows above the pairwise budget", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    x = seq_len(12),
    y = seq_len(12) / 10
  )
  config <- list(
    predictors = c("x", "y"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = NULL,
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "full",
    repeats = 1L,
    prop = 1,
    resample_seed = 1L,
    xdim = 2L,
    ydim = 2L,
    seeds = seq_len(1415L),
    rlen = 10L,
    k = 2L,
    cross_models = character()
  )

  result <- SOMevidence:::.gui_preflight_result(config, raw)
  expect_false(result$ok)
  expect_equal(result$prepared$model_budget, 1415L)
  expect_equal(result$prepared$planned_pairwise_comparisons, 1000405)
  expect_identical(result$prepared$max_pairwise_comparisons, 1000000L)
  expect_false(result$prepared$pairwise_budget_ok)
  expect_match(result$error, "exceeding the limit")
  status <- SOMevidence:::.format_gui_preflight_status(result)
  expect_match(status, "Preflight not ready")
  expect_match(status, "Pairwise budget: 1000405 planned; limit 1000000")
})

test_that("GUI preflight reports cross-model split prerequisites as review", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    domain = c(rep("a", 2), rep("b", 4), rep("c", 6)),
    x = seq_len(12),
    y = seq_len(12) / 10
  )
  config <- list(
    predictors = c("x", "y"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = "domain",
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "leave_domain_out",
    repeats = 2L,
    prop = 0.8,
    resample_seed = 1L,
    xdim = 3L,
    ydim = 3L,
    seeds = 1L,
    rlen = 10L,
    k = 2:8,
    cross_models = c("kmeans", "ward")
  )

  partial_preflight <- SOMevidence:::.gui_preflight_result(config, raw)
  expect_true(partial_preflight$ok)
  expect_identical(
    partial_preflight$prepared$cross_model_feasible_splits,
    1L
  )
  expect_match(
    paste(partial_preflight$prepared$notes, collapse = " "),
    "1 of 3 resampling splits"
  )
  partial_status <- SOMevidence:::.format_gui_preflight_status(
    partial_preflight
  )
  expect_match(partial_status, "Preflight ready with review items")
  expect_match(partial_status, "Cross-model prerequisites: 1/3")

  none_config <- config
  none_config$xdim <- 5L
  none_config$ydim <- 2L
  none_config$k <- 2:10
  none_preflight <- SOMevidence:::.gui_preflight_result(none_config, raw)
  expect_false(none_preflight$ok)
  expect_identical(none_preflight$prepared$cross_model_feasible_splits, 0L)
  expect_match(none_preflight$error, "None of the requested cross-model")
  expect_match(
    paste(none_preflight$prepared$notes, collapse = " "),
    "0 of 3 resampling splits"
  )
})

test_that("GUI preflight checks preprocessing within every analysis split", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    domain = rep(c("a", "b", "c"), each = 4),
    x = c(rep(1, 8), 2:5)
  )
  config <- list(
    predictors = "x",
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = "domain",
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "leave_domain_out",
    repeats = 2L,
    prop = 0.8,
    resample_seed = 1L,
    xdim = 2L,
    ydim = 2L,
    seeds = 1L,
    rlen = 10L,
    k = 2L,
    cross_models = character()
  )

  preflight <- SOMevidence:::.gui_preflight_result(config, raw)
  expect_true(preflight$ok)
  expect_identical(preflight$prepared$preprocessing_feasible_splits, 2L)
  expect_equal(nrow(preflight$prepared$preprocessing_failures), 1L)
  expect_identical(preflight$prepared$feasible_som_fits, 2L)
  expect_identical(preflight$prepared$infeasible_som_fits, 1L)
  expect_identical(
    preflight$prepared$structurally_ineligible_som_fits,
    0L
  )
  expect_identical(
    preflight$prepared$preprocessing_ineligible_som_fits,
    1L
  )
  expect_match(
    paste(preflight$prepared$notes, collapse = " "),
    "split-specific preprocessing"
  )
  expect_no_match(
    paste(preflight$prepared$notes, collapse = " "),
    "map units"
  )

  all_constant <- raw
  all_constant$x <- 1
  blocked <- SOMevidence:::.gui_preflight_result(config, all_constant)
  expect_false(blocked$ok)
  expect_identical(blocked$prepared$preprocessing_feasible_splits, 0L)
  expect_identical(blocked$prepared$feasible_som_fits, 0L)
  expect_match(blocked$error, "No planned SOM fit")
})

test_that("GUI cross-model preflight handles split-specific constants", {
  raw <- data.frame(
    sample_id = sprintf("s%02d", 1:12),
    domain = rep(c("a", "b", "c"), each = 4),
    constant_predictor = rep(1, 12),
    gradient = seq_len(12)
  )
  config <- list(
    predictors = c("constant_predictor", "gradient"),
    id_column = "sample_id",
    group_column = NULL,
    time_column = NULL,
    domain_column = "domain",
    weight_column = NULL,
    external_column = NULL,
    transform = "identity",
    center = TRUE,
    scale = TRUE,
    zero_replacement = NULL,
    resample_method = "leave_domain_out",
    repeats = 2L,
    prop = 0.8,
    resample_seed = 1L,
    xdim = 2L,
    ydim = 2L,
    seeds = 1L,
    rlen = 10L,
    k = 2:3,
    cross_models = "ward"
  )

  prepared <- SOMevidence:::.prepare_gui_analysis(config, raw)
  expect_identical(prepared$cross_model_feasible_splits, 3L)
  expect_match(
    SOMevidence:::.format_gui_preflight_status(list(
      ok = TRUE, prepared = prepared
    )),
    "3/3 splits pass split-specific preprocessing"
  )

  ensemble <- fit_som_ensemble(
    prepared$data,
    prepared$specification,
    prepared$resamples,
    preprocess = prepared$preprocessing,
    keep_models = FALSE
  )
  references <- fit_cross_models(
    ensemble, methods = "ward", k = prepared$specification$k
  )
  expect_equal(nrow(references$failures), 0L)
  expect_equal(length(references$records), 6L)
})

test_that("GUI CSV errors are actionable without exposing a local path", {
  private_path <- file.path(
    tempdir(), "private-monitoring-location", "missing.csv"
  )
  condition <- tryCatch(
    SOMevidence:::.read_gui_csv(private_path),
    error = identity
  )
  expect_s3_class(condition, "error")
  expect_match(conditionMessage(condition), "The CSV could not be read")
  expect_false(grepl(
    private_path, conditionMessage(condition), fixed = TRUE
  ))

  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data.frame(x = 1:2, y = 3:4), csv, row.names = FALSE)
  expect_identical(
    SOMevidence:::.read_gui_csv(csv),
    data.frame(x = 1:2, y = 3:4)
  )

  incomplete <- tempfile(fileext = ".csv")
  on.exit(unlink(incomplete), add = TRUE)
  writeBin(charToRaw("x,y\n1,2"), incomplete)
  expect_identical(
    SOMevidence:::.read_gui_csv(incomplete),
    data.frame(x = 1L, y = 2L)
  )
})

test_that("GUI diagnostics report every failure and warning stream", {
  workflow <- list(
    ensemble = list(
      fits = list(list(success = TRUE), list(success = FALSE)),
      expected_models = 2L,
      failures = data.frame(error = "SOM failed"),
      warnings = data.frame(warning = "SOM warning")
    ),
    consensus = list(k2 = list()),
    consensus_failures = data.frame(error = "Consensus failed"),
    cross_models = list(
      records = list(list()),
      failures = data.frame(error = "Reference failed"),
      warnings = data.frame(warning = "Reference warning")
    )
  )
  diagnostics <- SOMevidence:::.gui_workflow_diagnostics(workflow)
  expect_equal(sum(diagnostics$Count), 5L)
  expect_true(all(c("SOM ensemble", "Consensus", "Cross-model references") %in%
      diagnostics$Stream
  ))
  status <- SOMevidence:::.gui_workflow_status(workflow)
  expect_match(status, "1/2 succeeded")
  expect_match(status, "1 failed; 1 warning")
  expect_match(status, "1 computed; 1 not computed")

  labelled <- SOMevidence:::.gui_table_labels(data.frame(
    k = 2L, median_ari = 0.5, ari_q025 = 0.2
  ))
  expect_identical(names(labelled), c("k", "Median ARI", "ARI 2.5%"))
  choices <- SOMevidence:::.gui_consensus_choices(workflow)
  expect_identical(choices, c("k = 2" = "k2"))
})

test_that("guided GUI examples represent distinct study-design questions", {
  catalog <- SOMevidence:::.gui_example_catalog()
  expect_identical(
    names(catalog), c("clusters", "gradient", "grouped", "transfer")
  )
  expect_identical(
    SOMevidence:::.gui_example_choices(),
    stats::setNames(
      names(catalog), vapply(catalog, `[[`, character(1), "label")
    )
  )

  for (example_id in names(catalog)) {
    example <- SOMevidence:::.gui_builtin_example(example_id)
    expect_equal(nrow(example), 180L, info = example_id)
    expect_equal(sum(grepl("^indicator_", names(example))), 6L)
    expect_false(any(grepl("^environment_", names(example))))
    expect_true(anyDuplicated(example$sample_id) == 0L)

    source_lines <- SOMevidence:::.gui_example_source_lines(example_id)
    expect_error(parse(text = source_lines), NA, info = example_id)
    environment <- new.env(parent = asNamespace("SOMevidence"))
    eval(parse(text = source_lines), envir = environment)
    expect_equal(environment$raw, example, info = example_id)
  }

  expect_true(
    "external_label" %in%
      names(SOMevidence:::.gui_builtin_example("clusters"))
  )
  expect_false(
    "external_label" %in%
      names(SOMevidence:::.gui_builtin_example("gradient"))
  )
  expect_true(
    "sampling_group" %in%
      names(SOMevidence:::.gui_builtin_example("grouped"))
  )
  expect_true(
    "reporting_region" %in%
      names(SOMevidence:::.gui_builtin_example("transfer"))
  )

  expect_identical(
    SOMevidence:::.gui_example_defaults("grouped")$resample_method,
    "group_subsample"
  )
  expect_identical(
    SOMevidence:::.gui_example_defaults("transfer")$resample_method,
    "leave_domain_out"
  )
  expect_error(
    SOMevidence:::.gui_example_spec("unknown"),
    "available built-in example"
  )
})

test_that("GUI help separates evidence, action and interpretive boundaries", {
  guide <- SOMevidence:::.gui_metric_guide()
  expect_identical(nrow(guide), 4L)
  expect_identical(
    guide$`Evidence view`,
    c("Representation", "Partition stability", "Consensus", "Cross-model")
  )
  expect_match(
    guide$`Do not infer`[guide$`Evidence view` == "Cross-model"],
    "Accuracy"
  )

  for (view in c("audit", "partitions", "consensus", "cross_model")) {
    guidance <- SOMevidence:::.gui_view_guidance(view)
    expect_named(guidance, c("title", "question", "inspect", "boundary"))
    expect_true(all(nzchar(unlist(guidance))))
  }

  troubleshooting <- SOMevidence:::.gui_troubleshooting_guide()
  expect_true(nrow(troubleshooting) >= 5L)
  expect_true(all(nzchar(troubleshooting$Action)))

  template <- SOMevidence:::.gui_csv_template()
  expect_equal(nrow(template), 12L)
  expect_named(template, c(
    "sample_id", "sampling_group", "survey_date", "reporting_region",
    "measure_temperature", "measure_nutrient", "external_label"
  ))
  expect_true(anyDuplicated(template$sample_id) == 0L)
})

test_that("GUI bilingual labels and tooltips cover the guided workflow", {
  translations <- SOMevidence:::.gui_translations()
  tooltips <- SOMevidence:::.gui_tooltips()
  expect_identical(names(translations$en), names(translations$zh))
  expect_identical(names(tooltips$en), names(tooltips$zh))
  expect_true(all(nzchar(translations$en)))
  expect_true(all(nzchar(translations$zh)))
  expect_true(all(nzchar(tooltips$en)))
  expect_true(all(nzchar(tooltips$zh)))
  expect_identical(SOMevidence:::.gui_language("zh"), "zh")
  expect_identical(SOMevidence:::.gui_language("anything"), "en")
  expect_match(SOMevidence:::.gui_tr("zh", "run_workflow"), "运行")
  expect_match(tooltips$zh[["splitter"]], "拖动")

  chinese_choices <- SOMevidence:::.gui_example_choices("zh")
  expect_identical(unname(chinese_choices), names(
    SOMevidence:::.gui_example_catalog()
  ))
  expect_true(all(grepl("[一-鿿]", names(chinese_choices))))

  chinese_examples <- SOMevidence:::.gui_example_table("zh")
  expect_named(chinese_examples, c("示例", "设计问题", "推荐重抽样"))
  expect_true(all(grepl("[一-鿿]", chinese_examples[[1L]])))

  chinese_metrics <- SOMevidence:::.gui_metric_guide("zh")
  expect_named(chinese_metrics, c("证据视图", "主要问题", "不应推断"))
  chinese_troubleshooting <- SOMevidence:::.gui_troubleshooting_guide("zh")
  expect_named(chinese_troubleshooting, c("信息", "处理方法"))

  span <- SOMevidence:::.gui_i18n("candidate_k")
  expect_identical(span$attribs[["data-somevidence-tip"]], "candidate_k")
  expect_match(
    span$attribs[["data-somevidence-tooltip"]],
    "partition sizes"
  )
  expect_identical(span$attribs$tabindex, "0")

  app_source <- paste(deparse(body(launch_som_app)), collapse = "\n")
  expect_match(app_source, "somevidence-splitter", fixed = TRUE)
  expect_match(app_source, "pointerdown", fixed = TRUE)
  expect_match(app_source, "dblclick", fixed = TRUE)
  expect_match(app_source, "somevidence-sidebar-pct", fixed = TRUE)
  expect_match(app_source, "somevidence-tooltip", fixed = TRUE)
  expect_match(app_source, "aria-describedby", fixed = TRUE)
  expect_match(app_source, "targetFor", fixed = TRUE)
  expect_match(app_source, "label.querySelector", fixed = TRUE)
})

test_that("GUI localizes tables and status without changing evidence", {
  audit <- SOMevidence:::.gui_data_audit(
    data.frame(sample_id = c("a", "b"), x = 1:2),
    "x"
  )
  localized <- SOMevidence:::.gui_localize_data_audit(audit, "zh")
  expect_named(localized, c("检查项", "结果", "状态"))
  expect_true("就绪" %in% localized[["状态"]])

  english <- SOMevidence:::.gui_table_labels(data.frame(
    k = 2L, median_ari = 0.5
  ))
  chinese <- SOMevidence:::.gui_table_labels(
    data.frame(k = 2L, median_ari = 0.5), "zh"
  )
  expect_equal(unname(chinese[["ARI 中位数"]]), english[["Median ARI"]])
})

test_that("optional GUI constructs a Shiny application", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  expect_s3_class(launch_som_app(), "shiny.appobj")
})

test_that("GUI server runs a compact built-in reproducible workflow", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")
  app <- launch_som_app()

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      language = "en",
      data_source = "built_in",
      example_id = "clusters",
      predictors = c("indicator_01", "indicator_02", "indicator_03"),
      id_column = "sample_id",
      group_column = "None",
      time_column = "None",
      domain_column = "None",
      weight_column = "None",
      external_column = "external_label",
      transform = "identity",
      center = TRUE,
      scale = TRUE,
      resample_method = "subsample",
      repeats = 2,
      prop = 0.8,
      xdim = 3,
      ydim = 2,
      seeds = "1,2",
      rlen = 10,
      k = "2,3",
      cross_models = "ward",
      plot_type = "audit",
      run = 1
    )
    result <- analysis()
    expect_s3_class(result$workflow, "som_workflow")
    expect_identical(result$config$example_id, "clusters")
    expect_identical(result$config$resample_method, "subsample")
    expect_identical(result$config$seeds, 1:2)
    expect_true(nrow(result$workflow$partitions$stability) > 0L)
    expect_match(output$status, "SOM fits")
    expect_match(output$preflight_status, "Preflight ready")
    expect_match(output$preflight_status, "planned fits")
    expect_match(output$preflight_status, "eligible")
    expect_match(output$diagnostics_table, "Failures")
    example_summary <- paste(unlist(output$example_summary), collapse = " ")
    view_guidance <- paste(unlist(output$view_guidance), collapse = " ")
    expect_match(example_summary, "same classes reappear")
    expect_match(view_guidance, "Representation diagnostics")
    expect_match(output$metric_guide, "Do not infer")
    expect_match(output$troubleshooting_guide, "Select predictors")

    session$setInputs(language = "zh")
    expect_identical(input$language, "zh")
    expect_identical(
      input$predictors,
      c("indicator_01", "indicator_02", "indicator_03")
    )
    expect_identical(input$id_column, "sample_id")
    expect_identical(input$external_column, "external_label")
    expect_identical(result$config$seeds, 1:2)
    expect_match(output$preflight_status, "预检查就绪")
    expect_match(output$metric_guide, "不应推断")
    expect_match(output$troubleshooting_guide, "处理方法")
    expect_match(
      paste(unlist(output$view_guidance), collapse = " "),
      "表征质量诊断"
    )

    session$setInputs(consensus_k = "k3")
    expect_identical(input$consensus_k, "k3")
  })
})
