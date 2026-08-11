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
    cross_models = c("kmeans", "ward")
  )
  script <- SOMevidence:::.render_gui_script(config)

  expect_error(parse(text = script), NA)
  expect_true(any(grepl("group_subsample", script, fixed = TRUE)))
  expect_true(any(grepl("monitoring.csv", script, fixed = TRUE)))
  expect_true(any(grepl("file.path(\"data\"", script, fixed = TRUE)))
  expect_true(any(grepl("file.exists(input_path)", script, fixed = TRUE)))
  expect_true(any(grepl("read.csv(input_path", script, fixed = TRUE)))
  expect_true(any(grepl("cross_models", script, fixed = TRUE)))
  expect_true(any(grepl("time = raw[[\"date\"]]", script, fixed = TRUE)))
  expect_true(any(grepl(
    "weight = raw[[\"survey_weight\"]]", script, fixed = TRUE
  )))
  expect_true(any(grepl("required_SOMevidence_version", script, fixed = TRUE)))

  full_config <- config
  full_config$resample_method <- "full"
  full_script <- SOMevidence:::.render_gui_script(full_config)
  resample_line <- full_script[
    grepl("som_resamples", full_script, fixed = TRUE)
  ]
  expect_false(grepl("repeats", resample_line, fixed = TRUE))
  expect_false(grepl("prop", resample_line, fixed = TRUE))
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
    "Review"
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
  prepared <- SOMevidence:::.prepare_gui_analysis(config, raw)
  expect_s3_class(prepared$data, "som_data")
  expect_s3_class(prepared$resamples, "som_resamples")
  expect_equal(prepared$model_budget, 4L)
  expect_match(paste(prepared$notes, collapse = " "), "Missing predictors")
  expect_match(paste(prepared$notes, collapse = " "), "Constant predictors")

  raw$x[[2L]] <- Inf
  expect_error(
    SOMevidence:::.prepare_gui_analysis(config, raw),
    "infinite"
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
  expect_match(status, "1 failed; 1 warnings")

  labelled <- SOMevidence:::.gui_table_labels(data.frame(
    k = 2L, median_ari = 0.5, ari_q025 = 0.2
  ))
  expect_identical(names(labelled), c("k", "Median ARI", "ARI 2.5%"))
  choices <- SOMevidence:::.gui_consensus_choices(workflow)
  expect_identical(choices, c("k = 2" = "k2"))
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
      data_source = "built_in",
      predictors = c("environment_01", "environment_02", "environment_03"),
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
    expect_identical(result$config$resample_method, "subsample")
    expect_identical(result$config$seeds, 1:2)
    expect_true(nrow(result$workflow$partitions$stability) > 0L)
    expect_match(output$status, "SOM fits")
    expect_match(output$preflight_status, "Preflight passed")
    expect_match(output$diagnostics_table, "Failures")

    session$setInputs(consensus_k = "k3")
    expect_identical(input$consensus_k, "k3")
  })
})
