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
    .abort(sprintf("`%s` must be comma-separated integers of at least %d.", name, lower))
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
    if (!is.numeric(weights) || anyNA(weights) || any(weights < 0)) {
      .abort("The selected weight column must be numeric, non-missing and non-negative.")
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

.code_value <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")

.render_gui_script <- function(config) {
  metadata_expression <- function(column) {
    if (is.null(column)) "NULL" else sprintf("raw[[%s]]", .code_value(column))
  }
  source_lines <- if (config$data_source == "built_in") {
    c(
      "simulated <- simulate_som_scenario(\"clusters\", n = 180, p = 6, seed = 1)",
      "raw <- as.data.frame(simulated$layers$environment)",
      "raw$sample_id <- simulated$metadata$id",
      "raw$external_label <- simulated$metadata$external_label"
    )
  } else {
    sprintf(
      "raw <- utils::read.csv(%s, check.names = FALSE)",
      .code_value(config$input_file)
    )
  }
  resample_arguments <- c(
    "data",
    sprintf("method = %s", .code_value(config$resample_method)),
    sprintf("repeats = %dL", config$repeats),
    sprintf("prop = %s", .code_value(config$prop)),
    sprintf("seed = %dL", config$resample_seed)
  )
  if (config$resample_method == "group_subsample") {
    resample_arguments <- c(resample_arguments, "unit = \"group\"")
  }
  if (config$resample_method == "leave_domain_out") {
    resample_arguments <- c(resample_arguments, "domain = \"domain\"")
  }
  package_version <- config$package_version %||%
    as.character(utils::packageVersion("SOMevidence"))

  c(
    sprintf("# GUI configuration schema: %d", config$schema_version %||% 1L),
    sprintf("# Generated with SOMevidence %s", package_version),
    "library(SOMevidence)",
    sprintf(
      "required_SOMevidence_version <- %s",
      .code_value(package_version)
    ),
    paste0(
      "if (!identical(as.character(utils::packageVersion(\"SOMevidence\")), ",
      "required_SOMevidence_version)) warning(\"The installed SOMevidence version ",
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
    sprintf("  external_label = %s", metadata_expression(config$external_column)),
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
    sprintf("  cross_models = %s", .code_value(config$cross_models)),
    ")",
    "workflow"
  )
}

#' Launch the optional reproducible SOM interface
#'
#' The Shiny interface exposes a compact subset of the package workflow for
#' teaching and exploratory configuration. Every completed run can export its
#' exact R script and a YAML configuration. The exported script, not the GUI
#' session, is the reproducible analysis record.
#'
#' @section Lifecycle:
#' Experimental. The interface and its exported configuration schema may
#' change after independent usability testing. The ordinary R API remains the
#' authoritative analysis interface.
#'
#' @return A running Shiny application. This function is interactive and does
#'   not return an analysis object to the calling session.
#' @export
launch_som_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    .abort("Install the suggested package `shiny` to launch the interface.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .abort("Install the suggested package `ggplot2` to launch the interface.")
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("SOM evidence workflow"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          "data_source", "Data source",
          choices = c("Built-in simulation" = "built_in", "Upload CSV" = "upload")
        ),
        shiny::conditionalPanel(
          "input.data_source == 'upload'",
          shiny::fileInput("file", "CSV file", accept = ".csv")
        ),
        shiny::selectizeInput("predictors", "Numeric predictors", choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput("id_column", "Sample ID", choices = "None"),
        shiny::selectInput("group_column", "Sampling group", choices = "None"),
        shiny::selectInput("time_column", "Sampling time", choices = "None"),
        shiny::selectInput("domain_column", "Transfer domain", choices = "None"),
        shiny::selectInput(
          "weight_column", "Survey/summary weight (metadata only)",
          choices = "None"
        ),
        shiny::selectInput("external_column", "External label", choices = "None"),
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
          choices = c("full", "subsample", "group_subsample", "leave_domain_out")
        ),
        shiny::numericInput("repeats", "Resample repeats", 5L, min = 1L),
        shiny::numericInput("prop", "Analysis proportion", 0.8, min = 0.1, max = 1),
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
        shiny::downloadButton("download_yaml", "Export YAML")
      ),
      shiny::mainPanel(
        shiny::radioButtons(
          "plot_type", "Evidence view",
          choices = c(
            "Representation" = "audit",
            "Partition stability" = "partitions",
            "Consensus" = "consensus",
            "Cross-model agreement" = "cross_model"
          )
        ),
        shiny::verbatimTextOutput("status"),
        shiny::plotOutput("evidence_plot", height = "520px"),
        shiny::h4("Partition stability"),
        shiny::tableOutput("partition_table"),
        shiny::h4("Cross-model agreement"),
        shiny::tableOutput("cross_table")
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
      utils::read.csv(input$file$datapath, check.names = FALSE)
    })
    shiny::observeEvent(raw_data(), {
      raw <- raw_data()
      numeric_columns <- names(raw)[vapply(raw, is.numeric, logical(1))]
      metadata_defaults <- .gui_metadata_defaults(names(raw))
      default_predictors <- .gui_default_predictors(raw)
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
    analysis <- shiny::eventReactive(input$run, {
      raw <- raw_data()
      shiny::validate(shiny::need(length(input$predictors) > 0L, "Select predictors."))
      config <- list(
        schema_version = 1L,
        package_version = as.character(utils::packageVersion("SOMevidence")),
        data_source = input$data_source,
        input_file = if (input$data_source == "upload") {
          input$file$name
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
        repeats = .gui_integer(input$repeats, "repeats"),
        prop = input$prop,
        resample_seed = 1L,
        xdim = .gui_integer(input$xdim, "xdim", lower = 2L),
        ydim = .gui_integer(input$ydim, "ydim", lower = 2L),
        seeds = .parse_integer_set(input$seeds, "seeds"),
        rlen = .gui_integer(input$rlen, "rlen", lower = 10L),
        k = .parse_integer_set(input$k, "k", lower = 2L),
        cross_models = input$cross_models
      )
      .validate_gui_config(config, raw)
      values <- lapply(
        config[c(
          "id_column", "group_column", "time_column", "domain_column",
          "weight_column", "external_column"
        )],
        function(column) if (is.null(column)) NULL else raw[[column]]
      )
      data <- som_data(
        x = raw[, config$predictors, drop = FALSE],
        id = values$id_column,
        group = values$group_column,
        time = values$time_column,
        domain = values$domain_column,
        weight = values$weight_column,
        external_label = values$external_column
      )
      resample_arguments <- list(
        data = data,
        method = config$resample_method,
        repeats = config$repeats,
        prop = config$prop,
        seed = config$resample_seed
      )
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
      workflow <- shiny::withProgress(message = "Fitting ensemble", value = 0.3, {
        run_som_workflow(
          data,
          specification,
          resamples,
          preprocess = som_preprocess(
            config$transform,
            center = config$center,
            scale = config$scale,
            zero_replacement = config$zero_replacement
          ),
          cross_models = config$cross_models,
          keep_models = FALSE
        )
      })
      list(workflow = workflow, config = config)
    })

    output$status <- shiny::renderPrint({
      print(analysis()$workflow)
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
      plot(workflow$consensus[[1L]])
    })
    output$partition_table <- shiny::renderTable({
      analysis()$workflow$partitions$stability
    }, digits = 3)
    output$cross_table <- shiny::renderTable({
      comparison <- analysis()$workflow$cross_comparison
      if (is.null(comparison)) return(data.frame(note = "Not requested"))
      comparison$summary
    }, digits = 3)
    output$download_r <- shiny::downloadHandler(
      filename = function() "som_workflow.R",
      content = function(file) {
        writeLines(.render_gui_script(analysis()$config), file)
      }
    )
    output$download_yaml <- shiny::downloadHandler(
      filename = function() "som_workflow.yml",
      content = function(file) {
        if (!requireNamespace("yaml", quietly = TRUE)) {
          .abort("Install `yaml` to export the configuration.")
        }
        yaml::write_yaml(analysis()$config, file)
      }
    )
  }
  shiny::shinyApp(ui, server)
}
