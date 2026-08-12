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

.gui_language <- function(language) {
  if (identical(language, "zh")) "zh" else "en"
}

.gui_translations <- function() {
  en <- c(
    app_title = "SOM evidence workflow",
    app_subtitle = paste0(
      "A guided interface for configuring, checking and interpreting a ",
      "reproducible SOM ensemble analysis."
    ),
    section_data = "1. Data and study design",
    data_source = "Data source",
    guided_example = "Guided example",
    reset_example = "Reset recommended settings",
    download_example = "Download example CSV",
    csv_file = "CSV file",
    download_template = "Download CSV template",
    upload_help = paste0(
      "Predictors are not selected automatically. Review numeric IDs, ",
      "coordinates, dates and design variables before choosing features."
    ),
    predictors = "Numeric predictors",
    simulated_predictor_note = paste0(
      "Built-in columns indicator_01 to indicator_06 are generic simulated ",
      "measurements with no fixed ecological meaning. Uploaded names are ",
      "kept unchanged."
    ),
    sample_id = "Sample ID",
    sampling_group = "Sampling group",
    sampling_time = "Sampling time",
    transfer_domain = "Transfer domain",
    weight = "Survey/summary weight (metadata only)",
    external_label = "External label",
    metadata_help = paste0(
      "Metadata and external labels stay outside the SOM training matrix. ",
      "Weight is not used as a training weight."
    ),
    section_preprocess = "2. Preprocessing",
    transformation = "Transformation",
    center = "Center variables",
    scale = "Scale variables",
    zero_replacement = "CLR zero replacement",
    preprocess_help = paste0(
      "Transformations and scaling are estimated within each analysis ",
      "split. Choose them from measurement properties."
    ),
    section_resampling = "3. Resampling design",
    resampling = "Resampling",
    repeats = "Resample repeats",
    proportion = "Analysis proportion",
    resampling_help = paste0(
      "Resample the independent sampling unit. Keep nested records together ",
      "and leave out whole domains for transfer questions."
    ),
    section_ensemble = "4. SOM ensemble",
    grid_width = "Grid width",
    grid_height = "Grid height",
    som_seeds = "SOM seeds",
    training_iterations = "Training iterations",
    candidate_k = "Candidate k",
    section_export = "5. Triangulation and export",
    cross_models = "Cross-model references",
    run_workflow = "Run workflow",
    export_r = "Export R script",
    export_yaml = "Export configuration snapshot",
    export_help = paste0(
      "The R script is executable. The YAML file records the controls but ",
      "cannot currently restore them. Use R for final and larger analyses."
    ),
    tab_results = "Run and results",
    tab_interpretation = "Interpretation guide",
    tab_examples = "Examples and help",
    data_preflight = "Data preflight",
    preflight_prompt = "Resolve every 'Not ready' item before running.",
    evidence_view = "Evidence view",
    consensus_solution = "Consensus solution",
    run_diagnostics = "Run diagnostics",
    partition_stability = "Partition stability",
    cross_model_agreement = "Cross-model agreement",
    interpretation_heading = "Read the evidence streams separately",
    interpretation_intro = paste0(
      "Representation, partition stability, sample consensus and ",
      "cross-model agreement answer different questions."
    ),
    preflight_messages = "Common preflight messages",
    reporting_boundary = "Reporting boundary",
    reporting_text = paste0(
      "Report what was prespecified, which perturbations were used, how many ",
      "fits succeeded and where evidence disagreed. ARI is not accuracy."
    ),
    examples_heading = "Guided teaching examples",
    examples_intro = paste0(
      "These simulations isolate common analysis questions. They are ",
      "teaching cases, not performance benchmarks."
    ),
    workflow_heading = "From the interface to a reproducible analysis",
    workflow_step_1 = "Start with the example closest to the study design.",
    workflow_step_2 = "Inspect the data audit and choose predictors explicitly.",
    workflow_step_3 = "Match resampling to the independent sampling unit.",
    workflow_step_4 = "Run each evidence view and record disagreements.",
    workflow_step_5 = "Export the R script and continue the final analysis in R.",
    guide_link = "Open the full Shiny interface guide",
    question = "Question: ",
    design = "Design: ",
    learning_goal = "Learning goal: ",
    inspect = "Inspect: ",
    do_not_infer = "Do not infer: ",
    interpretive_boundary = "Interpretive boundary",
    footer = paste0(
      "Experimental interface | Designed for a local R session | ",
      "SOMevidence sends no telemetry."
    )
  )
  zh <- c(
    app_title = "SOM \u8bc1\u636e\u5de5\u4f5c\u6d41",
    app_subtitle = "\u7528\u4e8e\u914d\u7f6e\u3001\u68c0\u67e5\u548c\u89e3\u8bfb\u53ef\u590d\u73b0 SOM \u96c6\u6210\u5206\u6790\u7684\u5f15\u5bfc\u5f0f\u754c\u9762\u3002",
    section_data = "1. \u6570\u636e\u4e0e\u7814\u7a76\u8bbe\u8ba1",
    data_source = "\u6570\u636e\u6765\u6e90",
    guided_example = "\u5f15\u5bfc\u793a\u4f8b",
    reset_example = "\u6062\u590d\u63a8\u8350\u8bbe\u7f6e",
    download_example = "\u4e0b\u8f7d\u793a\u4f8b CSV",
    csv_file = "CSV \u6587\u4ef6",
    download_template = "\u4e0b\u8f7d CSV \u6a21\u677f",
    upload_help = paste0(
      "\u7cfb\u7edf\u4e0d\u4f1a\u81ea\u52a8\u9009\u62e9\u9884\u6d4b\u53d8\u91cf\u3002\u8bf7\u5148\u68c0\u67e5\u6570\u5b57\u578b\u7f16\u53f7\u3001\u5750\u6807\u3001\u65e5\u671f\u548c\u8bbe\u8ba1\u53d8\u91cf\uff0c",
      "\u518d\u9009\u62e9\u7279\u5f81\u3002"
    ),
    predictors = "\u6570\u503c\u9884\u6d4b\u53d8\u91cf",
    simulated_predictor_note = paste0(
      "\u5185\u7f6e\u793a\u4f8b\u4e2d\u7684 indicator_01 \u81f3 indicator_06 \u4e3a\u901a\u7528\u6a21\u62df\u6307\u6807\uff0c",
      "\u4e0d\u5bf9\u5e94\u56fa\u5b9a\u751f\u6001\u542b\u4e49\uff1b\u4e0a\u4f20\u6570\u636e\u7684\u539f\u59cb\u5217\u540d\u4fdd\u6301\u4e0d\u53d8\u3002"
    ),
    sample_id = "\u6837\u672c\u7f16\u53f7",
    sampling_group = "\u62bd\u6837\u5206\u7ec4",
    sampling_time = "\u62bd\u6837\u65f6\u95f4",
    transfer_domain = "\u8fc1\u79fb\u57df",
    weight = "\u8c03\u67e5/\u6c47\u603b\u6743\u91cd\uff08\u4ec5\u4f5c\u5143\u6570\u636e\uff09",
    external_label = "\u5916\u90e8\u6807\u7b7e",
    metadata_help = paste0(
      "\u5143\u6570\u636e\u548c\u5916\u90e8\u6807\u7b7e\u4e0d\u8fdb\u5165 SOM \u8bad\u7ec3\u77e9\u9635\u3002\u6743\u91cd\u4e0d\u4f5c\u4e3a\u8bad\u7ec3\u6743\u91cd\u4f7f\u7528\u3002"
    ),
    section_preprocess = "2. \u9884\u5904\u7406",
    transformation = "\u6570\u636e\u53d8\u6362",
    center = "\u4e2d\u5fc3\u5316",
    scale = "\u6807\u51c6\u5316",
    zero_replacement = "CLR \u96f6\u503c\u66ff\u4ee3\u503c",
    preprocess_help = paste0(
      "\u6570\u636e\u53d8\u6362\u548c\u6807\u51c6\u5316\u53c2\u6570\u5728\u6bcf\u4e2a\u5206\u6790\u5b50\u96c6\u5185\u4f30\u8ba1\u3002\u8bf7\u6839\u636e\u6d4b\u91cf\u5c5e\u6027\u9009\u62e9\u65b9\u6cd5\u3002"
    ),
    section_resampling = "3. \u91cd\u62bd\u6837\u8bbe\u8ba1",
    resampling = "\u91cd\u62bd\u6837\u65b9\u6cd5",
    repeats = "\u91cd\u590d\u6b21\u6570",
    proportion = "\u5206\u6790\u6837\u672c\u6bd4\u4f8b",
    resampling_help = paste0(
      "\u5e94\u91cd\u62bd\u6837\u72ec\u7acb\u62bd\u6837\u5355\u5143\u3002\u5d4c\u5957\u8bb0\u5f55\u5e94\u4fdd\u6301\u5728\u540c\u4e00\u7ec4\u5185\uff1b\u8fc1\u79fb\u95ee\u9898\u5e94\u6574\u4e2a\u7559\u51fa\u76d1\u6d4b\u57df\u3002"
    ),
    section_ensemble = "4. SOM \u96c6\u6210",
    grid_width = "\u7f51\u683c\u5bbd\u5ea6",
    grid_height = "\u7f51\u683c\u9ad8\u5ea6",
    som_seeds = "SOM \u968f\u673a\u79cd\u5b50",
    training_iterations = "\u8bad\u7ec3\u8fed\u4ee3\u6b21\u6570",
    candidate_k = "\u5019\u9009 k \u503c",
    section_export = "5. \u4ea4\u53c9\u9a8c\u8bc1\u4e0e\u5bfc\u51fa",
    cross_models = "\u5bf9\u7167\u6a21\u578b",
    run_workflow = "\u8fd0\u884c\u5de5\u4f5c\u6d41",
    export_r = "\u5bfc\u51fa R \u811a\u672c",
    export_yaml = "\u5bfc\u51fa\u914d\u7f6e\u5feb\u7167",
    export_help = paste0(
      "R \u811a\u672c\u53ef\u76f4\u63a5\u6267\u884c\u3002YAML \u6587\u4ef6\u8bb0\u5f55\u754c\u9762\u8bbe\u7f6e\uff0c\u4f46\u76ee\u524d\u4e0d\u80fd\u7528\u4e8e\u6062\u590d\u63a7\u4ef6\u3002",
      "\u6b63\u5f0f\u5206\u6790\u548c\u5927\u89c4\u6a21\u8ba1\u7b97\u8bf7\u4f7f\u7528 R\u3002"
    ),
    tab_results = "\u8fd0\u884c\u4e0e\u7ed3\u679c",
    tab_interpretation = "\u7ed3\u679c\u89e3\u8bfb",
    tab_examples = "\u793a\u4f8b\u4e0e\u5e2e\u52a9",
    data_preflight = "\u6570\u636e\u9884\u68c0\u67e5",
    preflight_prompt = "\u8fd0\u884c\u524d\u8bf7\u5904\u7406\u6240\u6709\u201c\u672a\u5c31\u7eea\u201d\u9879\u76ee\u3002",
    evidence_view = "\u8bc1\u636e\u89c6\u56fe",
    consensus_solution = "\u5171\u8bc6\u5206\u7ec4",
    run_diagnostics = "\u8fd0\u884c\u8bca\u65ad",
    partition_stability = "\u5206\u7ec4\u7a33\u5b9a\u6027",
    cross_model_agreement = "\u8de8\u6a21\u578b\u4e00\u81f4\u6027",
    interpretation_heading = "\u5206\u522b\u89e3\u8bfb\u5404\u7c7b\u8bc1\u636e",
    interpretation_intro = paste0(
      "\u8868\u5f81\u8d28\u91cf\u3001\u5206\u7ec4\u7a33\u5b9a\u6027\u3001\u6837\u672c\u5171\u8bc6\u548c\u8de8\u6a21\u578b\u4e00\u81f4\u6027\u56de\u7b54\u4e0d\u540c\u95ee\u9898\u3002"
    ),
    preflight_messages = "\u5e38\u89c1\u9884\u68c0\u67e5\u4fe1\u606f",
    reporting_boundary = "\u62a5\u544a\u8fb9\u754c",
    reporting_text = paste0(
      "\u8bf7\u62a5\u544a\u9884\u5148\u8bbe\u5b9a\u7684\u65b9\u6848\u3001\u6240\u7528\u6270\u52a8\u65b9\u5f0f\u3001\u6210\u529f\u62df\u5408\u6570\u91cf\u53ca\u8bc1\u636e\u4e0d\u4e00\u81f4\u4e4b\u5904\u3002",
      "ARI \u4e0d\u662f\u51c6\u786e\u7387\u3002"
    ),
    examples_heading = "\u5f15\u5bfc\u6559\u5b66\u793a\u4f8b",
    examples_intro = "\u8fd9\u4e9b\u6a21\u62df\u6570\u636e\u7528\u4e8e\u8bf4\u660e\u5e38\u89c1\u5206\u6790\u95ee\u9898\uff0c\u4e0d\u662f\u6027\u80fd\u6d4b\u8bd5\u57fa\u51c6\u3002",
    workflow_heading = "\u4ece\u754c\u9762\u8bbe\u7f6e\u5230\u53ef\u590d\u73b0\u5206\u6790",
    workflow_step_1 = "\u5148\u9009\u62e9\u4e0e\u7814\u7a76\u8bbe\u8ba1\u6700\u63a5\u8fd1\u7684\u793a\u4f8b\u3002",
    workflow_step_2 = "\u68c0\u67e5\u6570\u636e\u5ba1\u8ba1\u7ed3\u679c\uff0c\u5e76\u660e\u786e\u9009\u62e9\u9884\u6d4b\u53d8\u91cf\u3002",
    workflow_step_3 = "\u4f7f\u91cd\u62bd\u6837\u5355\u5143\u4e0e\u72ec\u7acb\u62bd\u6837\u5355\u5143\u4e00\u81f4\u3002",
    workflow_step_4 = "\u9010\u4e00\u67e5\u770b\u8bc1\u636e\u89c6\u56fe\uff0c\u5e76\u8bb0\u5f55\u4e0d\u4e00\u81f4\u7684\u7ed3\u679c\u3002",
    workflow_step_5 = "\u5bfc\u51fa R \u811a\u672c\uff0c\u5e76\u5728 R \u4e2d\u5b8c\u6210\u6b63\u5f0f\u5206\u6790\u3002",
    guide_link = "\u6253\u5f00\u5b8c\u6574 Shiny \u754c\u9762\u6307\u5357",
    question = "\u95ee\u9898\uff1a",
    design = "\u8bbe\u8ba1\uff1a",
    learning_goal = "\u5b66\u4e60\u76ee\u6807\uff1a",
    inspect = "\u91cd\u70b9\u67e5\u770b\uff1a",
    do_not_infer = "\u4e0d\u5e94\u63a8\u65ad\uff1a",
    interpretive_boundary = "\u89e3\u8bfb\u8fb9\u754c",
    footer = "\u5b9e\u9a8c\u6027\u754c\u9762 | \u5efa\u8bae\u5728\u672c\u5730 R \u4f1a\u8bdd\u4e2d\u8fd0\u884c | SOMevidence \u4e0d\u53d1\u9001\u4efb\u4f55\u9065\u6d4b\u6570\u636e\u3002"
  )
  stopifnot(identical(names(en), names(zh)))
  list(en = en, zh = zh)
}

.gui_tr <- function(language, key) {
  language <- .gui_language(language)
  translations <- .gui_translations()[[language]]
  value <- unname(translations[[key]])
  if (is.null(value)) key else value
}

.gui_tooltips <- function() {
  en <- c(
    data_source = "Use a teaching simulation or upload one rectangular CSV.",
    guided_example = "Each example answers a different study-design question.",
    predictors = "Measured numeric variables used to train the SOM.",
    sample_id = "A stable unique identifier used to trace each observation.",
    sampling_group = "The independent unit kept together during group resampling.",
    sampling_time = "A date, campaign or ordered time field retained as metadata.",
    transfer_domain = "A region, campaign or instrument left out as one domain.",
    weight = "Retained for summaries; it does not weight SOM training.",
    external_label = "A known label used only after unsupervised fitting.",
    transformation = "Apply a prespecified transformation before scaling.",
    center = "Subtract the training-split mean from each variable.",
    scale = "Divide each variable by its training-split standard deviation.",
    resampling = "Choose how the analysis data are perturbed across repeats.",
    repeats = "Number of independently generated resampling splits.",
    proportion = "Fraction of rows or groups retained in each analysis split.",
    grid_width = "Number of SOM units across the horizontal grid dimension.",
    grid_height = "Number of SOM units across the vertical grid dimension.",
    som_seeds = "Random starts used to assess sensitivity to initialization.",
    training_iterations = "Training updates used for each SOM fit.",
    candidate_k = "Hard partition sizes assessed after fitting the maps.",
    cross_models = "Reference methods fitted on the same eligible splits.",
    data_preflight = "Checks data, design and model feasibility before fitting.",
    evidence_view = "Switch among distinct evidence questions; do not merge them.",
    run_diagnostics = "Counts and examples of warnings or failed computations.",
    partition_stability = "Agreement among candidate partitions across perturbations.",
    cross_model_agreement = "Agreement with prespecified reference algorithms.",
    splitter = paste0(
      "Drag to resize the panels. Use arrow keys for small steps; double-",
      "click to reset."
    )
  )
  zh <- c(
    data_source = "\u53ef\u9009\u62e9\u6559\u5b66\u6a21\u62df\u6570\u636e\uff0c\u6216\u4e0a\u4f20\u4e00\u4e2a\u77e9\u5f62 CSV \u6587\u4ef6\u3002",
    guided_example = "\u6bcf\u4e2a\u793a\u4f8b\u5206\u522b\u5bf9\u5e94\u4e00\u7c7b\u7814\u7a76\u8bbe\u8ba1\u95ee\u9898\u3002",
    predictors = "\u7528\u4e8e\u8bad\u7ec3 SOM \u7684\u5b9e\u6d4b\u6570\u503c\u53d8\u91cf\u3002",
    sample_id = "\u7528\u4e8e\u8ffd\u8e2a\u6bcf\u4e2a\u89c2\u6d4b\u8bb0\u5f55\u7684\u7a33\u5b9a\u552f\u4e00\u7f16\u53f7\u3002",
    sampling_group = "\u5206\u7ec4\u91cd\u62bd\u6837\u65f6\u4fdd\u6301\u5b8c\u6574\u7684\u72ec\u7acb\u62bd\u6837\u5355\u5143\u3002",
    sampling_time = "\u4f5c\u4e3a\u5143\u6570\u636e\u4fdd\u7559\u7684\u65e5\u671f\u3001\u8c03\u67e5\u671f\u6216\u6709\u5e8f\u65f6\u95f4\u5b57\u6bb5\u3002",
    transfer_domain = "\u4f5c\u4e3a\u6574\u4f53\u7559\u51fa\u7684\u533a\u57df\u3001\u8c03\u67e5\u671f\u6216\u4eea\u5668\u57df\u3002",
    weight = "\u4ec5\u7528\u4e8e\u6c47\u603b\uff0c\u4e0d\u4f5c\u4e3a SOM \u8bad\u7ec3\u6743\u91cd\u3002",
    external_label = "\u4ec5\u5728\u65e0\u76d1\u7763\u62df\u5408\u5b8c\u6210\u540e\u4f7f\u7528\u7684\u5df2\u77e5\u6807\u7b7e\u3002",
    transformation = "\u5728\u6807\u51c6\u5316\u524d\u5e94\u7528\u9884\u5148\u8bbe\u5b9a\u7684\u6570\u636e\u53d8\u6362\u3002",
    center = "\u6bcf\u4e2a\u53d8\u91cf\u51cf\u53bb\u5f53\u524d\u8bad\u7ec3\u5b50\u96c6\u7684\u5747\u503c\u3002",
    scale = "\u6bcf\u4e2a\u53d8\u91cf\u9664\u4ee5\u5f53\u524d\u8bad\u7ec3\u5b50\u96c6\u7684\u6807\u51c6\u5dee\u3002",
    resampling = "\u8bbe\u5b9a\u91cd\u590d\u5206\u6790\u65f6\u5982\u4f55\u6270\u52a8\u5206\u6790\u6570\u636e\u3002",
    repeats = "\u72ec\u7acb\u751f\u6210\u7684\u91cd\u62bd\u6837\u5206\u6790\u5b50\u96c6\u6570\u91cf\u3002",
    proportion = "\u6bcf\u4e2a\u5206\u6790\u5b50\u96c6\u4fdd\u7559\u7684\u884c\u6216\u5206\u7ec4\u6bd4\u4f8b\u3002",
    grid_width = "SOM \u7f51\u683c\u6c34\u5e73\u65b9\u5411\u7684\u5355\u5143\u6570\u3002",
    grid_height = "SOM \u7f51\u683c\u5782\u76f4\u65b9\u5411\u7684\u5355\u5143\u6570\u3002",
    som_seeds = "\u7528\u4e8e\u8bc4\u4f30\u968f\u673a\u521d\u59cb\u5316\u654f\u611f\u6027\u7684\u968f\u673a\u79cd\u5b50\u3002",
    training_iterations = "\u6bcf\u4e2a SOM \u62df\u5408\u7684\u8bad\u7ec3\u66f4\u65b0\u6b21\u6570\u3002",
    candidate_k = "\u5730\u56fe\u8bad\u7ec3\u5b8c\u6210\u540e\u8bc4\u4f30\u7684\u786c\u5206\u7ec4\u6570\u91cf\u3002",
    cross_models = "\u5728\u76f8\u540c\u53ef\u7528\u5206\u6790\u5b50\u96c6\u4e0a\u62df\u5408\u7684\u5bf9\u7167\u65b9\u6cd5\u3002",
    data_preflight = "\u62df\u5408\u524d\u68c0\u67e5\u6570\u636e\u3001\u7814\u7a76\u8bbe\u8ba1\u548c\u6a21\u578b\u53ef\u6267\u884c\u6027\u3002",
    evidence_view = "\u5728\u4e0d\u540c\u8bc1\u636e\u95ee\u9898\u4e4b\u95f4\u5207\u6362\uff0c\u4e0d\u5e94\u5c06\u5b83\u4eec\u5408\u5e76\u4e3a\u5355\u4e00\u5f97\u5206\u3002",
    run_diagnostics = "\u6c47\u603b\u8b66\u544a\u548c\u8ba1\u7b97\u5931\u8d25\u7684\u6570\u91cf\u53ca\u793a\u4f8b\u3002",
    partition_stability = "\u5019\u9009\u5206\u7ec4\u5728\u591a\u6b21\u6270\u52a8\u4e2d\u7684\u4e00\u81f4\u7a0b\u5ea6\u3002",
    cross_model_agreement = "SOM \u4e0e\u9884\u5148\u8bbe\u5b9a\u5bf9\u7167\u7b97\u6cd5\u7684\u4e00\u81f4\u7a0b\u5ea6\u3002",
    splitter = "\u62d6\u52a8\u53ef\u8c03\u8282\u5de6\u53f3\u680f\u5bbd\u5ea6\uff1b\u65b9\u5411\u952e\u53ef\u5fae\u8c03\uff0c\u53cc\u51fb\u6062\u590d\u9ed8\u8ba4\u6bd4\u4f8b\u3002"
  )
  stopifnot(identical(names(en), names(zh)))
  list(en = en, zh = zh)
}

.gui_table_term_tooltips <- function(language = "en") {
  if (.gui_language(language) == "zh") {
    return(c(
      "ARI \u4e2d\u4f4d\u6570" = "ARI \u8861\u91cf\u4e24\u4e2a\u5206\u7ec4\u7684\u4e00\u81f4\u6027\u5e76\u6821\u6b63\u968f\u673a\u4e00\u81f4\uff0c\u4e0d\u662f\u51c6\u786e\u7387\u3002",
      "AMI \u4e2d\u4f4d\u6570" = "AMI \u7528\u4e92\u4fe1\u606f\u8861\u91cf\u5206\u7ec4\u4e00\u81f4\u6027\u5e76\u6821\u6b63\u968f\u673a\u4e00\u81f4\u3002",
      "\u5171\u540c\u6837\u672c\u8986\u76d6\u7387\u4e2d\u4f4d\u6570" = "\u6bcf\u6b21\u6210\u5bf9\u6bd4\u8f83\u4e2d\u540c\u65f6\u53ef\u8bc4\u4f30\u6837\u672c\u7684\u6bd4\u4f8b\u3002",
      "\u6210\u5458\u652f\u6301\u5ea6" = "\u5728\u53ef\u8bc4\u4f30\u7684\u91cd\u590d\u62df\u5408\u4e2d\uff0c\u6837\u672c\u88ab\u5206\u5230\u5171\u8bc6\u7c7b\u522b\u7684\u6bd4\u4f8b\u3002",
      "\u5f52\u5c5e\u71b5" = "\u8861\u91cf\u6837\u672c\u5206\u7ec4\u5f52\u5c5e\u7684\u5206\u6563\u7a0b\u5ea6\uff1b\u503c\u8d8a\u9ad8\uff0c\u5f52\u5c5e\u8d8a\u4e0d\u786e\u5b9a\u3002"
    ))
  }
  c(
    "Median ARI" = paste0(
      "Adjusted Rand index: chance-corrected partition agreement, not ",
      "classification accuracy."
    ),
    "Median AMI" = paste0(
      "Adjusted mutual information: information-based agreement corrected ",
      "for chance."
    ),
    "Median joint coverage" = paste0(
      "The proportion of samples jointly evaluable in each pairwise ",
      "comparison."
    ),
    "Membership support" = paste0(
      "The proportion of evaluable repeated fits assigning a sample to its ",
      "consensus group."
    ),
    "Assignment entropy" = paste0(
      "Dispersion of a sample's assignments; higher values indicate greater ",
      "uncertainty."
    )
  )
}

.gui_i18n <- function(key) {
  tips <- .gui_tooltips()$en
  tip <- if (key %in% names(tips)) unname(tips[[key]]) else NULL
  shiny::tags$span(
    "data-somevidence-i18n" = key,
    "data-somevidence-tip" = if (is.null(tip)) NULL else key,
    "data-somevidence-tooltip" = tip,
    tabindex = if (is.null(tip)) NULL else "0",
    .gui_tr("en", key)
  )
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
  missing_status <- if (incomplete_rows) "Not ready" else "Ready"
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

.gui_localize_data_audit <- function(audit, language = "en") {
  if (.gui_language(language) == "en") return(audit)
  checks <- c(
    "Data source" = "\u6570\u636e\u6765\u6e90",
    "Rows" = "\u884c\u6570",
    "Columns" = "\u5217\u6570",
    "Numeric columns" = "\u6570\u503c\u5217\u6570",
    "Selected predictors" = "\u5df2\u9009\u9884\u6d4b\u53d8\u91cf",
    "Rows with missing predictor values" = "\u9884\u6d4b\u53d8\u91cf\u6709\u7f3a\u5931\u503c\u7684\u884c",
    "Rows with no observed predictor" = "\u6240\u6709\u9884\u6d4b\u53d8\u91cf\u5747\u7f3a\u5931\u7684\u884c",
    "Non-finite predictor values" = "\u975e\u6709\u9650\u9884\u6d4b\u53d8\u91cf\u503c",
    "Constant predictors" = "\u5e38\u91cf\u9884\u6d4b\u53d8\u91cf"
  )
  statuses <- c(
    "Ready" = "\u5c31\u7eea", "Not ready" = "\u672a\u5c31\u7eea",
    "Select predictors" = "\u8bf7\u9009\u62e9\u9884\u6d4b\u53d8\u91cf",
    "Resolve" = "\u9700\u5904\u7406", "Review" = "\u9700\u590d\u6838"
  )
  audit$Check <- ifelse(
    audit$Check %in% names(checks), unname(checks[audit$Check]), audit$Check
  )
  audit$Status <- ifelse(
    audit$Status %in% names(statuses),
    unname(statuses[audit$Status]), audit$Status
  )
  audit$Result[audit$Result == "None"] <- "\u65e0"
  names(audit) <- c("\u68c0\u67e5\u9879", "\u7ed3\u679c", "\u72b6\u6001")
  audit
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
  if (anyNA(predictor_matrix)) {
    .abort(paste0(
      "Predictors selected in the experimental GUI must be complete. ",
      "Handle missingness in a controlled analysis or use the R API with ",
      "an explicitly justified `max_na_fraction`."
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
  resample_capture <- .capture_warnings(
    do.call(som_resamples, resample_arguments)
  )
  if (inherits(resample_capture$value, "error")) {
    stop(resample_capture$value)
  }
  resamples <- resample_capture$value
  specification <- som_spec(
    c(config$xdim, config$ydim),
    seeds = config$seeds,
    rlen = config$rlen,
    k = config$k
  )

  split_analysis_n <- vapply(
    resamples$splits, function(split) length(split$analysis), integer(1)
  )
  preprocessing_by_layer <- .normalise_preprocess(
    preprocessing, names(data$layers)
  )
  split_preprocessing_checks <- lapply(resamples$splits, function(split) {
    tryCatch(
      {
        training_layers <- Map(
          function(layer, layer_preprocess) {
            .fit_preprocessor(
              layer[split$analysis, , drop = FALSE], layer_preprocess
            )$data
          },
          data$layers,
          preprocessing_by_layer
        )
        if (any(vapply(training_layers, function(layer) {
          any(rowMeans(is.na(layer)) > specification$max_na_fraction)
        }, logical(1)))) {
          .abort("At least one training row exceeds `max_na_fraction`.")
        }
        .resolve_layer_weights(
          training_layers,
          specification$layer_weights,
          specification$normalize_layers
        )
        list(ok = TRUE, error = NA_character_)
      },
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
  })
  split_preprocessing_feasible <- vapply(
    split_preprocessing_checks, `[[`, logical(1), "ok"
  )
  preprocessing_failures <- data.frame(
    split_id = vapply(resamples$splits, `[[`, character(1), "id")[
      !split_preprocessing_feasible
    ],
    error = vapply(
      split_preprocessing_checks, `[[`, character(1), "error"
    )[!split_preprocessing_feasible],
    stringsAsFactors = FALSE
  )
  grid_units <- specification$grids$xdim * specification$grids$ydim
  split_grid_structural <- outer(split_analysis_n, grid_units, `>=`)
  split_grid_feasible <- split_grid_structural &
    split_preprocessing_feasible
  feasible_som_fits <- as.integer(
    sum(split_grid_feasible) * length(specification$seeds)
  )
  model_budget <- length(resamples$splits) *
    nrow(expand_som_spec(specification))
  infeasible_som_fits <- as.integer(model_budget - feasible_som_fits)
  structural_ineligible_fits <- as.integer(
    sum(!split_grid_structural) * length(specification$seeds)
  )
  preprocess_ineligible_fits <- as.integer(
    sum(split_grid_structural & !split_preprocessing_feasible) *
      length(specification$seeds)
  )
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

  notes <- resample_capture$warnings$warning
  if (nrow(preprocessing_failures)) {
    notes <- c(
      notes,
      sprintf(
        paste0(
          "%d of %d resampling splits fail split-specific preprocessing or ",
          "layer-weight prerequisites. First reason: %s"
        ),
        nrow(preprocessing_failures),
        length(resamples$splits),
        preprocessing_failures$error[[1L]]
      )
    )
  }
  if (structural_ineligible_fits > 0L && feasible_som_fits > 0L) {
    notes <- c(
      notes,
      sprintf(
        paste0(
          "%d of %d planned SOM fits do not meet the structural prerequisite ",
          "that analysis rows must be at least the number of map units."
        ),
        structural_ineligible_fits, model_budget
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
    structurally_ineligible_som_fits = structural_ineligible_fits,
    preprocessing_ineligible_som_fits = preprocess_ineligible_fits,
    preprocessing_feasible_splits = as.integer(sum(
      split_preprocessing_feasible
    )),
    preprocessing_failures = preprocessing_failures,
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
            "No planned SOM fit meets all preprocessing, layer-weight and ",
            "sample-size prerequisites. Review the split diagnostics, use a ",
            "smaller grid or revise the analysis design."
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
      if (prepared$duplicate_analysis_splits > 0L) {
        return(list(
          ok = FALSE,
          prepared = prepared,
          error = paste0(
            prepared$duplicate_analysis_splits,
            " resampling split(s) repeat earlier analysis rows and would ",
            "overweight the same data perturbation. Reduce the number of ",
            "repeats or revise the resampling design."
          )
        ))
      }
      if (!is.na(prepared$cross_model_feasible_splits) &&
            prepared$cross_model_feasible_splits == 0L) {
        return(list(
          ok = FALSE,
          prepared = prepared,
          error = paste0(
            "None of the requested cross-model reference fits meets the ",
            "split-specific preprocessing and sample-size prerequisites. ",
            "Revise the predictors, candidate k values or resampling design."
          )
        ))
      }
      list(ok = TRUE, prepared = prepared)
    },
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
}

.format_gui_preflight_zh <- function(result) {
  if (!isTRUE(result$ok)) {
    lines <- "\u9884\u68c0\u67e5\u672a\u5c31\u7eea\u3002"
    if (!is.null(result$prepared)) {
      prepared <- result$prepared
      lines <- c(
        lines,
        sprintf(
          "SOM \u53ef\u6267\u884c\u6027\uff1a\u8ba1\u5212 %d \u4e2a\u62df\u5408\uff0c%d \u4e2a\u53ef\u6267\u884c\uff0c%d \u4e2a\u4e0d\u53ef\u6267\u884c\u3002",
          prepared$model_budget,
          prepared$feasible_som_fits,
          prepared$infeasible_som_fits
        ),
        sprintf(
          "\u6210\u5bf9\u6bd4\u8f83\u9884\u7b97\uff1a\u8ba1\u5212 %s\uff0c\u4e0a\u9650 %s\u3002",
          format(prepared$planned_pairwise_comparisons, trim = TRUE),
          format(prepared$max_pairwise_comparisons, trim = TRUE)
        )
      )
    }
    return(paste(
      c(lines, paste0("\u9700\u5904\u7406\uff08\u6280\u672f\u4fe1\u606f\uff09\uff1a", result$error)),
      collapse = "\n"
    ))
  }

  prepared <- result$prepared
  lines <- c(
    if (length(prepared$notes)) "\u9884\u68c0\u67e5\u5c31\u7eea\uff0c\u4f46\u6709\u9879\u76ee\u9700\u590d\u6838\u3002" else "\u9884\u68c0\u67e5\u5c31\u7eea\u3002",
    sprintf(
      "%d \u4e2a\u6837\u672c\uff1b%d \u4e2a\u9884\u6d4b\u53d8\u91cf\uff1b%d \u4e2a\u91cd\u62bd\u6837\u5206\u6790\u5b50\u96c6\u3002",
      nrow(prepared$data$metadata),
      ncol(prepared$data$layers$data),
      length(prepared$resamples$splits)
    ),
    sprintf(
      "SOM \u53ef\u6267\u884c\u6027\uff1a\u8ba1\u5212 %d \u4e2a\u62df\u5408\uff0c%d \u4e2a\u53ef\u6267\u884c\uff0c%d \u4e2a\u4e0d\u53ef\u6267\u884c\u3002",
      prepared$model_budget,
      prepared$feasible_som_fits,
      prepared$infeasible_som_fits
    ),
    sprintf(
      "\u6210\u5bf9\u6bd4\u8f83\u9884\u7b97\uff1a\u8ba1\u5212 %s\uff0c\u4e0a\u9650 %s\u3002",
      format(prepared$planned_pairwise_comparisons, trim = TRUE),
      format(prepared$max_pairwise_comparisons, trim = TRUE)
    )
  )
  if (!is.na(prepared$cross_model_feasible_splits)) {
    lines <- c(lines, sprintf(
      paste0(
        "\u5bf9\u7167\u6a21\u578b\u524d\u63d0\uff1a%d/%d \u4e2a\u5206\u6790\u5b50\u96c6\u901a\u8fc7\u5b50\u96c6\u5185\u9884\u5904\u7406\u68c0\u67e5\uff0c",
        "\u4e14\u6837\u672c\u6570\u5927\u4e8e\u6700\u5927\u5019\u9009 k\uff08%d\uff09\u3002"
      ),
      prepared$cross_model_feasible_splits,
      length(prepared$resamples$splits),
      max(prepared$specification$k)
    ))
  }
  lines <- c(lines, sprintf(
    "\u9664\u9996\u6b21\u5916\uff0c\u91cd\u590d\u7684\u5206\u6790\u5b50\u96c6\uff1a%d\u3002",
    prepared$duplicate_analysis_splits
  ))
  if (length(prepared$notes)) {
    lines <- c(lines, paste0("\u9700\u590d\u6838\uff08\u6280\u672f\u4fe1\u606f\uff09\uff1a", prepared$notes))
  }
  paste(lines, collapse = "\n")
}

.format_gui_preflight_status <- function(result, language = "en") {
  if (.gui_language(language) == "zh") {
    return(.format_gui_preflight_zh(result))
  }
  if (!isTRUE(result$ok)) {
    lines <- c("Preflight not ready.")
    if (!is.null(result$prepared)) {
      prepared <- result$prepared
      lines <- c(
        lines,
        sprintf(
          paste0(
            "SOM eligibility: %d planned fits; %d eligible; ",
            "%d ineligible."
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
        "SOM eligibility: %d planned fits; %d eligible; ",
        "%d ineligible."
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

.gui_workflow_diagnostics <- function(workflow, language = "en") {
  language <- .gui_language(language)
  translated <- function(en, zh) if (language == "zh") zh else en
  issue_row <- function(stream, type, table, field) {
    count <- if (is.data.frame(table)) nrow(table) else 0L
    detail <- if (count && field %in% names(table)) {
      paste(
        utils::head(unique(as.character(table[[field]])), 2L),
        collapse = " | "
      )
    } else {
      translated("None", "\u65e0")
    }
    data.frame(
      Stream = stream, Type = type, Count = count, Example = detail,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  rows <- list(
    issue_row(
      translated("SOM ensemble", "SOM \u96c6\u6210"),
      translated("Failures", "\u5931\u8d25"), workflow$ensemble$failures, "error"
    ),
    issue_row(
      translated("SOM ensemble", "SOM \u96c6\u6210"),
      translated("Warnings", "\u8b66\u544a"), workflow$ensemble$warnings, "warning"
    ),
    issue_row(
      translated("Consensus", "\u5171\u8bc6\u5206\u7ec4"),
      translated("Failures", "\u5931\u8d25"), workflow$consensus_failures, "error"
    )
  )
  if (!is.null(workflow$cross_models)) {
    rows <- c(rows, list(
      issue_row(
        translated("Cross-model references", "\u5bf9\u7167\u6a21\u578b"),
        translated("Failures", "\u5931\u8d25"),
        workflow$cross_models$failures, "error"
      ),
      issue_row(
        translated("Cross-model references", "\u5bf9\u7167\u6a21\u578b"),
        translated("Warnings", "\u8b66\u544a"),
        workflow$cross_models$warnings, "warning"
      )
    ))
  }
  result <- do.call(rbind, rows)
  if (language == "zh") {
    names(result) <- c("\u5206\u6790\u73af\u8282", "\u7c7b\u578b", "\u6570\u91cf", "\u793a\u4f8b")
  }
  result
}

.gui_workflow_status <- function(workflow, language = "en") {
  successful_som <- sum(vapply(
    workflow$ensemble$fits, function(x) isTRUE(x$success), logical(1)
  ))
  if (.gui_language(language) == "zh") {
    lines <- c(
      sprintf(
        "SOM \u62df\u5408\uff1a%d/%d \u6210\u529f\uff1b%d \u5931\u8d25\uff1b%d \u4e2a\u8b66\u544a\u3002",
        successful_som, workflow$ensemble$expected_models,
        nrow(workflow$ensemble$failures), nrow(workflow$ensemble$warnings)
      ),
      sprintf(
        "\u5171\u8bc6\u5206\u7ec4\uff1a%d \u4e2a\u5df2\u8ba1\u7b97\uff1b%d \u4e2a\u672a\u8ba1\u7b97\u3002",
        length(workflow$consensus), nrow(workflow$consensus_failures)
      )
    )
    if (!is.null(workflow$cross_models)) {
      lines <- c(lines, sprintf(
        "\u5bf9\u7167\u6a21\u578b\uff1a%d \u6210\u529f\uff1b%d \u5931\u8d25\uff1b%d \u4e2a\u8b66\u544a\u3002",
        length(workflow$cross_models$records),
        nrow(workflow$cross_models$failures),
        nrow(workflow$cross_models$warnings)
      ))
    } else {
      lines <- c(lines, "\u5bf9\u7167\u6a21\u578b\uff1a\u672a\u8bf7\u6c42\u3002")
    }
    return(paste(lines, collapse = "\n"))
  }
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

.gui_table_labels <- function(x, language = "en") {
  if (!is.data.frame(x)) return(x)
  labels_en <- c(
    k = "k", scope = "Evidence scope", n_pairs = "Pairs",
    n_pairs_evaluable = "Evaluable pairs", median_joint_n = "Median joint n",
    median_joint_coverage = "Median joint coverage",
    median_ari = "Median ARI", ari_q025 = "ARI 2.5%",
    ari_q975 = "ARI 97.5%", median_ami = "Median AMI",
    ami_q025 = "AMI 2.5%", ami_q975 = "AMI 97.5%",
    method = "Reference method", n_comparisons = "Comparisons"
  )
  labels_zh <- c(
    k = "k", scope = "\u8bc1\u636e\u8303\u56f4", n_pairs = "\u6210\u5bf9\u6570",
    n_pairs_evaluable = "\u53ef\u8bc4\u4f30\u6210\u5bf9\u6570",
    median_joint_n = "\u5171\u540c\u6837\u672c\u6570\u4e2d\u4f4d\u6570",
    median_joint_coverage = "\u5171\u540c\u6837\u672c\u8986\u76d6\u7387\u4e2d\u4f4d\u6570",
    median_ari = "ARI \u4e2d\u4f4d\u6570", ari_q025 = "ARI 2.5%",
    ari_q975 = "ARI 97.5%", median_ami = "AMI \u4e2d\u4f4d\u6570",
    ami_q025 = "AMI 2.5%", ami_q975 = "AMI 97.5%",
    method = "\u5bf9\u7167\u65b9\u6cd5", n_comparisons = "\u6bd4\u8f83\u6570"
  )
  labels <- if (.gui_language(language) == "zh") labels_zh else labels_en
  original <- names(x)
  fallback <- tools::toTitleCase(gsub("_", " ", original, fixed = TRUE))
  replacement <- unname(labels[original])
  names(x) <- ifelse(is.na(replacement), fallback, replacement)
  rownames(x) <- NULL
  x
}

.gui_example_catalog <- function() {
  common <- list(
    transform = "identity", center = TRUE, scale = TRUE,
    repeats = 6L, prop = 0.8, xdim = 6L, ydim = 4L,
    seeds = 1:3, rlen = 300L, k = 2:4,
    cross_models = c("kmeans", "ward")
  )
  list(
    clusters = c(common, list(
      label = "Discrete classes: do the same groups reappear?",
      label_zh = "\u79bb\u6563\u7c7b\u522b\uff1a\u76f8\u540c\u5206\u7ec4\u80fd\u5426\u91cd\u73b0\uff1f",
      question = paste0(
        "Do the same classes reappear across repeated SOM fits, and do ",
        "K-means and Ward.D2 show a similar pattern?"
      ),
      question_zh = paste0(
        "\u76f8\u540c\u7c7b\u522b\u80fd\u5426\u5728\u591a\u6b21 SOM \u62df\u5408\u4e2d\u91cd\u73b0\uff0c\u4e14 K-means \u548c ",
        "Ward.D2 \u662f\u5426\u663e\u793a\u76f8\u4f3c\u683c\u5c40\uff1f"
      ),
      design = paste0(
        "Six variables form three clear simulated groups. Each repeat uses ",
        "a different subset of rows."
      ),
      design_zh = "6 \u4e2a\u53d8\u91cf\u5f62\u6210 3 \u4e2a\u6e05\u6670\u7684\u6a21\u62df\u7c7b\u522b\uff1b\u6bcf\u6b21\u91cd\u590d\u4f7f\u7528\u4e0d\u540c\u7684\u884c\u5b50\u96c6\u3002",
      lesson = paste0(
        "Read map quality, partition stability, sample support and ",
        "cross-model agreement separately."
      ),
      lesson_zh = "\u5206\u522b\u89e3\u8bfb\u5730\u56fe\u8d28\u91cf\u3001\u5206\u7ec4\u7a33\u5b9a\u6027\u3001\u6837\u672c\u652f\u6301\u5ea6\u548c\u8de8\u6a21\u578b\u4e00\u81f4\u6027\u3002",
      boundary = paste0(
        "Known simulated labels are used only after fitting. Agreement with ",
        "them is not part of SOM training."
      ),
      boundary_zh = "\u6a21\u62df\u6807\u7b7e\u4ec5\u5728\u62df\u5408\u5b8c\u6210\u540e\u4f7f\u7528\uff0c\u4e0d\u53c2\u4e0e SOM \u8bad\u7ec3\u3002",
      scenario = "clusters",
      simulation_args = list(n = 180L, p = 6L, seed = 101L),
      metadata = "external_label",
      resample_method = "subsample"
    )),
    gradient = c(common, list(
      label = "Continuous gradient: is a hard split supported?",
      label_zh = "\u8fde\u7eed\u68af\u5ea6\uff1a\u662f\u5426\u652f\u6301\u786c\u5206\u7ec4\uff1f",
      question = paste0(
        "Can the SOM represent a gradient well even when no clear class ",
        "boundary exists?"
      ),
      question_zh = "\u5f53\u6570\u636e\u6ca1\u6709\u6e05\u6670\u7c7b\u522b\u8fb9\u754c\u65f6\uff0cSOM \u80fd\u5426\u4ecd\u826f\u597d\u8868\u5f81\u8fde\u7eed\u68af\u5ea6\uff1f",
      design = paste0(
        "Six related variables change along one continuous gradient. Each ",
        "repeat uses a different subset of rows."
      ),
      design_zh = "6 \u4e2a\u76f8\u5173\u53d8\u91cf\u6cbf\u540c\u4e00\u8fde\u7eed\u68af\u5ea6\u53d8\u5316\uff1b\u6bcf\u6b21\u91cd\u590d\u4f7f\u7528\u4e0d\u540c\u7684\u884c\u5b50\u96c6\u3002",
      lesson = paste0(
        "A useful map does not automatically justify a categorical ecological ",
        "interpretation."
      ),
      lesson_zh = "\u6709\u7528\u7684 SOM \u8868\u5f81\u5e76\u4e0d\u81ea\u52a8\u610f\u5473\u7740\u5e94\u5f53\u4f5c\u786c\u5206\u7ec4\u89e3\u8bfb\u3002",
      boundary = paste0(
        "The latent gradient is retained as simulation truth but never enters ",
        "the training matrix."
      ),
      boundary_zh = "\u6f5c\u5728\u8fde\u7eed\u68af\u5ea6\u4ec5\u4f5c\u4e3a\u6a21\u62df\u771f\u503c\u4fdd\u7559\uff0c\u4e0d\u8fdb\u5165\u8bad\u7ec3\u77e9\u9635\u3002",
      scenario = "gradient",
      simulation_args = list(n = 180L, p = 6L, seed = 102L),
      metadata = character(),
      resample_method = "subsample"
    )),
    grouped = c(common, list(
      label = "Grouped monitoring: resample whole sites",
      label_zh = "\u5206\u7ec4\u76d1\u6d4b\uff1a\u6309\u5b8c\u6574\u7ad9\u70b9\u91cd\u62bd\u6837",
      question = paste0(
        "How do results change when repeated records from the same site stay ",
        "together?"
      ),
      question_zh = "\u5f53\u540c\u4e00\u7ad9\u70b9\u7684\u91cd\u590d\u8bb0\u5f55\u4fdd\u6301\u5728\u4e00\u8d77\u65f6\uff0c\u5206\u6790\u7ed3\u679c\u5982\u4f55\u53d8\u5316\uff1f",
      design = paste0(
        "Records are nested within 30 simulated sites and share a site ",
        "effect. Whole sites are resampled."
      ),
      design_zh = "\u8bb0\u5f55\u5d4c\u5957\u4e8e 30 \u4e2a\u6a21\u62df\u7ad9\u70b9\uff0c\u5e76\u5171\u4eab\u7ad9\u70b9\u6548\u5e94\uff1b\u91cd\u62bd\u6837\u65f6\u4fdd\u7559\u5b8c\u6574\u7ad9\u70b9\u3002",
      lesson = paste0(
        "The resampling unit should follow the study design rather than treat ",
        "nested records as independent."
      ),
      lesson_zh = "\u91cd\u62bd\u6837\u5355\u5143\u5e94\u4e0e\u7814\u7a76\u8bbe\u8ba1\u4e2d\u7684\u72ec\u7acb\u62bd\u6837\u5355\u5143\u4e00\u81f4\u3002",
      boundary = paste0(
        "The requested group correlation is conditional within generated ",
        "classes, not a marginal correlation after class separation."
      ),
      boundary_zh = "\u8bbe\u5b9a\u7684\u7ec4\u5185\u76f8\u5173\u662f\u751f\u6210\u7c7b\u522b\u5185\u7684\u6761\u4ef6\u76f8\u5173\uff0c\u4e0d\u662f\u6df7\u5408\u7c7b\u522b\u5747\u503c\u540e\u7684\u8fb9\u9645\u76f8\u5173\u3002",
      scenario = "grouped_pseudoreplication",
      simulation_args = list(
        n = 180L, p = 6L, seed = 103L, n_groups = 30L, group_icc = 0.7
      ),
      metadata = c("group", "external_label"),
      resample_method = "group_subsample"
    )),
    transfer = c(common, list(
      label = "Domain transfer: leave out one monitoring domain",
      label_zh = "\u76d1\u6d4b\u57df\u8fc1\u79fb\uff1a\u6bcf\u6b21\u7559\u51fa\u4e00\u4e2a\u76d1\u6d4b\u57df",
      question = paste0(
        "How well does a map trained on other domains cover the domain left ",
        "out?"
      ),
      question_zh = "\u4f7f\u7528\u5176\u4ed6\u76d1\u6d4b\u57df\u8bad\u7ec3\u7684\u5730\u56fe\uff0c\u5bf9\u7559\u51fa\u57df\u7684\u8986\u76d6\u60c5\u51b5\u5982\u4f55\uff1f",
      design = paste0(
        "Three domains occupy successive ranges of a latent gradient and ",
        "receive an additional shift in the first two variables."
      ),
      design_zh = "3 \u4e2a\u76d1\u6d4b\u57df\u4f4d\u4e8e\u6f5c\u5728\u68af\u5ea6\u7684\u4e0d\u540c\u533a\u95f4\uff0c\u524d 2 \u4e2a\u53d8\u91cf\u8fd8\u9644\u52a0\u4e86\u57df\u504f\u79fb\u3002",
      lesson = paste0(
        "Leave-domain-out diagnostics describe mapping coverage and domain ",
        "shift; they are not prediction accuracy."
      ),
      lesson_zh = "\u7559\u57df\u5916\u8bca\u65ad\u53cd\u6620\u6620\u5c04\u8986\u76d6\u548c\u57df\u504f\u79fb\uff0c\u4e0d\u662f\u9884\u6d4b\u51c6\u786e\u7387\u3002",
      boundary = paste0(
        "This example combines range extrapolation with an additive shift and ",
        "does not isolate a causal mechanism."
      ),
      boundary_zh = "\u8be5\u793a\u4f8b\u540c\u65f6\u5305\u542b\u53d6\u503c\u8303\u56f4\u5916\u63a8\u548c\u9644\u52a0\u504f\u79fb\uff0c\u4e0d\u80fd\u7528\u4e8e\u8bc6\u522b\u56e0\u679c\u673a\u5236\u3002",
      scenario = "gradient",
      simulation_args = list(
        n = 180L, p = 6L, seed = 104L,
        n_domains = 3L, domain_shift = 0.75
      ),
      metadata = "domain",
      resample_method = "leave_domain_out"
    ))
  )
}

.gui_example_value <- function(specification, field, language = "en") {
  language <- .gui_language(language)
  localized <- if (language == "zh") paste0(field, "_zh") else field
  specification[[localized]] %||% specification[[field]]
}

.gui_example_choices <- function(language = "en") {
  catalog <- .gui_example_catalog()
  stats::setNames(
    names(catalog),
    vapply(
      catalog, .gui_example_value, character(1),
      field = "label", language = language
    )
  )
}

.gui_example_spec <- function(example_id) {
  catalog <- .gui_example_catalog()
  if (!is.character(example_id) || length(example_id) != 1L ||
        is.na(example_id) || !example_id %in% names(catalog)) {
    .abort("Select one available built-in example.")
  }
  catalog[[example_id]]
}

.gui_builtin_example <- function(example_id = "clusters") {
  specification <- .gui_example_spec(example_id)
  simulated <- do.call(
    simulate_som_scenario,
    c(list(scenario = specification$scenario), specification$simulation_args)
  )
  raw <- as.data.frame(simulated$layers[[1L]])
  names(raw) <- sub("^environment_", "indicator_", names(raw))
  raw$sample_id <- simulated$metadata$id
  if ("group" %in% specification$metadata) {
    raw$sampling_group <- simulated$metadata$group
  }
  if ("domain" %in% specification$metadata) {
    raw$reporting_region <- simulated$metadata$domain
  }
  if ("external_label" %in% specification$metadata) {
    raw$external_label <- simulated$metadata$external_label
  }
  raw
}

.gui_example_defaults <- function(example_id) {
  specification <- .gui_example_spec(example_id)
  specification[c(
    "transform", "center", "scale", "resample_method", "repeats", "prop",
    "xdim", "ydim", "seeds", "rlen", "k", "cross_models"
  )]
}

.gui_example_table <- function(language = "en") {
  catalog <- .gui_example_catalog()
  language <- .gui_language(language)
  resampling_labels <- if (language == "zh") {
    c(
      subsample = "\u6309\u884c",
      group_subsample = "\u6309\u5b8c\u6574\u5206\u7ec4",
      leave_domain_out = "\u6bcf\u6b21\u7559\u51fa\u4e00\u4e2a\u76d1\u6d4b\u57df"
    )
  } else {
    c(
      subsample = "Rows",
      group_subsample = "Whole groups",
      leave_domain_out = "Leave one domain out"
    )
  }
  resampling <- vapply(catalog, `[[`, character(1), "resample_method")
  result <- data.frame(
    Example = vapply(
      catalog, .gui_example_value, character(1),
      field = "label", language = language
    ),
    "Design question" = vapply(
      catalog, .gui_example_value, character(1),
      field = "question", language = language
    ),
    "Recommended resampling" = unname(resampling_labels[resampling]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (language == "zh") names(result) <- c("\u793a\u4f8b", "\u8bbe\u8ba1\u95ee\u9898", "\u63a8\u8350\u91cd\u62bd\u6837")
  result
}

.gui_view_guidance <- function(view = "audit", language = "en") {
  guides_en <- list(
    audit = list(
      title = "Representation diagnostics",
      question = "Does the fitted map represent the analysis rows adequately?",
      inspect = paste0(
        "Quantization error, topographic error, empty-unit rate and ",
        "fit success across the prespecified ensemble."
      ),
      boundary = paste0(
        "Good representation diagnostics do not establish that a hard ",
        "partition is reproducible or ecologically real."
      )
    ),
    partitions = list(
      title = "Partition stability",
      question = "Do candidate hard partitions recur under perturbation?",
      inspect = paste0(
        "ARI and AMI together with joint sample coverage, intervals and ",
        "clusterwise evidence across candidate k values."
      ),
      boundary = paste0(
        "Internal agreement is conditional on the chosen variables, map ",
        "design, resampling scheme and partitioning rule."
      )
    ),
    consensus = list(
      title = "Sample-level consensus",
      question = "Which individual assignments are consistently supported?",
      inspect = paste0(
        "Membership support, assignment entropy, replicated coverage ",
        "and clusterwise Jaccard rather than only the displayed label."
      ),
      boundary = paste0(
        "A consensus label is an aligned summary of repeated assignments, not ",
        "a known ecological class or a probability of correctness."
      )
    ),
    cross_model = list(
      title = "Cross-model agreement",
      question = "Does the same partition geometry recur in reference methods?",
      inspect = paste0(
        "Agreement with prespecified K-means, Ward.D2 and any ",
        "requested GMM fits on the same eligible splits."
      ),
      boundary = paste0(
        "Agreement is triangulation, not accuracy, ecological truth, a model ",
        "ranking or permission to select the most favourable method."
      )
    )
  )
  guides_zh <- list(
    audit = list(
      title = "\u8868\u5f81\u8d28\u91cf\u8bca\u65ad",
      question = "\u62df\u5408\u7684\u5730\u56fe\u80fd\u5426\u5145\u5206\u8868\u5f81\u5206\u6790\u6837\u672c\uff1f",
      inspect = "\u91cf\u5316\u8bef\u5dee\u3001\u62d3\u6251\u8bef\u5dee\u3001\u7a7a\u5355\u5143\u6bd4\u4f8b\u53ca\u9884\u8bbe\u96c6\u6210\u4e2d\u7684\u62df\u5408\u6210\u529f\u60c5\u51b5\u3002",
      boundary = "\u826f\u597d\u7684\u8868\u5f81\u8bca\u65ad\u4e0d\u80fd\u5355\u72ec\u8bc1\u660e\u786c\u5206\u7ec4\u53ef\u91cd\u73b0\u6216\u5177\u6709\u751f\u6001\u771f\u5b9e\u6027\u3002"
    ),
    partitions = list(
      title = "\u5206\u7ec4\u7a33\u5b9a\u6027",
      question = "\u5019\u9009\u786c\u5206\u7ec4\u80fd\u5426\u5728\u6570\u636e\u6270\u52a8\u4e2d\u91cd\u73b0\uff1f",
      inspect = "ARI\u3001AMI\u3001\u5171\u540c\u6837\u672c\u8986\u76d6\u7387\u3001\u533a\u95f4\u53ca\u4e0d\u540c k \u4e0b\u7684\u7c7b\u522b\u8bc1\u636e\u3002",
      boundary = "\u5185\u90e8\u4e00\u81f4\u6027\u53d6\u51b3\u4e8e\u6240\u9009\u53d8\u91cf\u3001\u5730\u56fe\u8bbe\u8ba1\u3001\u91cd\u62bd\u6837\u65b9\u6848\u548c\u5206\u7ec4\u89c4\u5219\u3002"
    ),
    consensus = list(
      title = "\u6837\u672c\u5c42\u9762\u5171\u8bc6",
      question = "\u54ea\u4e9b\u6837\u672c\u7684\u5206\u7ec4\u5f52\u5c5e\u5f97\u5230\u4e00\u81f4\u652f\u6301\uff1f",
      inspect = "\u6210\u5458\u652f\u6301\u5ea6\u3001\u5f52\u5c5e\u71b5\u3001\u91cd\u590d\u8986\u76d6\u7387\u548c\u7c7b\u522b Jaccard\uff0c\u800c\u4e0d\u53ea\u770b\u663e\u793a\u7684\u6807\u7b7e\u3002",
      boundary = "\u5171\u8bc6\u6807\u7b7e\u662f\u591a\u6b21\u5f52\u5c5e\u7ed3\u679c\u7684\u5bf9\u9f50\u6c47\u603b\uff0c\u4e0d\u662f\u5df2\u77e5\u751f\u6001\u7c7b\u522b\u6216\u6b63\u786e\u6982\u7387\u3002"
    ),
    cross_model = list(
      title = "\u8de8\u6a21\u578b\u4e00\u81f4\u6027",
      question = "\u5bf9\u7167\u65b9\u6cd5\u4e2d\u662f\u5426\u91cd\u73b0\u76f8\u4f3c\u7684\u5206\u7ec4\u51e0\u4f55\u7ed3\u6784\uff1f",
      inspect = "\u5728\u76f8\u540c\u53ef\u7528\u5206\u6790\u5b50\u96c6\u4e0a\uff0cSOM \u4e0e K-means\u3001Ward.D2 \u53ca\u53ef\u9009 GMM \u7684\u4e00\u81f4\u6027\u3002",
      boundary = "\u6a21\u578b\u4e00\u81f4\u662f\u4ea4\u53c9\u5370\u8bc1\uff0c\u4e0d\u662f\u51c6\u786e\u7387\u3001\u751f\u6001\u771f\u503c\u6216\u6a21\u578b\u6392\u540d\u3002"
    )
  )
  guides <- if (.gui_language(language) == "zh") guides_zh else guides_en
  guides[[view]] %||% guides$audit
}

.gui_metric_guide <- function(language = "en") {
  if (.gui_language(language) == "zh") {
    return(data.frame(
      "\u8bc1\u636e\u89c6\u56fe" = c("\u8868\u5f81\u8d28\u91cf", "\u5206\u7ec4\u7a33\u5b9a\u6027", "\u6837\u672c\u5171\u8bc6", "\u8de8\u6a21\u578b"),
      "\u4e3b\u8981\u95ee\u9898" = c(
        "\u5730\u56fe\u80fd\u5426\u8868\u5f81\u89c2\u6d4b\u6570\u636e\uff1f",
        "\u786c\u5206\u7ec4\u80fd\u5426\u5728\u6270\u52a8\u4e2d\u91cd\u73b0\uff1f",
        "\u54ea\u4e9b\u6837\u672c\u5f52\u5c5e\u5f97\u5230\u91cd\u590d\u652f\u6301\uff1f",
        "\u5bf9\u7167\u65b9\u6cd5\u662f\u5426\u663e\u793a\u76f8\u4f3c\u51e0\u4f55\u7ed3\u6784\uff1f"
      ),
      "\u4e0d\u5e94\u63a8\u65ad" = c(
        "\u79bb\u6563\u7c7b\u522b\u4e00\u5b9a\u5b58\u5728",
        "\u5185\u90e8\u7a33\u5b9a\u7684\u5206\u7ec4\u5c31\u662f\u751f\u6001\u771f\u503c",
        "\u5171\u8bc6\u6807\u7b7e\u4e00\u5b9a\u6b63\u786e\u6216\u5df2\u7ecf\u5916\u90e8\u9a8c\u8bc1",
        "\u51c6\u786e\u7387\u3001\u56e0\u679c\u6027\u6216\u6a21\u578b\u6392\u540d"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    "Evidence view" = c(
      "Representation", "Partition stability", "Consensus", "Cross-model"
    ),
    "Primary question" = c(
      "Does the map represent the observations?",
      "Does a hard partition recur under perturbation?",
      "Which sample assignments are repeatedly supported?",
      "Does comparable geometry recur in reference methods?"
    ),
    "Do not infer" = c(
      "That discrete classes exist",
      "That internally stable groups are ecological truth",
      "That a label is correct or externally validated",
      "Accuracy, causality or a model ranking"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.gui_troubleshooting_guide <- function(language = "en") {
  if (.gui_language(language) == "zh") {
    return(data.frame(
      "\u4fe1\u606f" = c(
        "\u8bf7\u9009\u62e9\u9884\u6d4b\u53d8\u91cf", "\u9884\u6d4b\u53d8\u91cf\u5fc5\u987b\u5b8c\u6574",
        "\u6ca1\u6709\u53ef\u6267\u884c\u7684 SOM \u62df\u5408", "\u5206\u6790\u5b50\u96c6\u91cd\u590d",
        "\u6ca1\u6709\u53ef\u6267\u884c\u7684\u5bf9\u7167\u6a21\u578b\u5206\u6790\u5b50\u96c6"
      ),
      "\u5904\u7406\u65b9\u6cd5" = c(
        "\u9009\u62e9\u5b9e\u6d4b\u6570\u503c\u7279\u5f81\uff0c\u6392\u9664\u7f16\u53f7\u3001\u65e5\u671f\u548c\u5750\u6807\u3002",
        "\u5728 R \u4e2d\u4f7f\u7528\u6709\u79d1\u5b66\u4f9d\u636e\u7684\u7f3a\u5931\u6570\u636e\u6d41\u7a0b\uff1b\u754c\u9762\u4e0d\u4f1a\u9690\u5f0f\u63d2\u8865\u3002",
        "\u51cf\u5c0f\u7f51\u683c\uff0c\u6216\u590d\u6838\u5b50\u96c6\u5185\u9884\u5904\u7406\u548c\u6837\u672c\u6570\u3002",
        "\u51cf\u5c11\u91cd\u590d\u6b21\u6570\uff0c\u6216\u4fee\u6539\u91cd\u62bd\u6837\u8bbe\u8ba1\u3002",
        "\u590d\u6838\u9884\u6d4b\u53d8\u91cf\u3001\u5019\u9009 k \u53ca\u5b50\u96c6\u6837\u672c\u6570\u3002"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Message = c(
      "Select predictors", "Predictors must be complete",
      "No planned SOM fit is eligible", "Repeated analysis sets",
      "No cross-model split is eligible"
    ),
    Action = c(
      "Choose measured numeric features; exclude IDs, dates and coordinates.",
      paste0(
        "Use a justified missing-data workflow in R; the GUI does not hide ",
        "imputation or missing-distance choices."
      ),
      "Reduce the grid or revise split-specific preprocessing and sample size.",
      "Reduce repeats or revise the resampling design.",
      "Review predictors, candidate k and split-specific sample size."
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.gui_csv_template <- function() {
  data.frame(
    sample_id = sprintf("sample_%02d", seq_len(12L)),
    sampling_group = rep(sprintf("site_%02d", 1:4), each = 3L),
    survey_date = rep(c("2026-04-15", "2026-07-15", "2026-10-15"), 4L),
    reporting_region = rep(c("region_a", "region_b"), each = 6L),
    measure_temperature = seq(10, 21, length.out = 12L),
    measure_nutrient = seq(0.05, 0.60, length.out = 12L),
    external_label = rep(c("reference_a", "reference_b"), each = 6L),
    stringsAsFactors = FALSE
  )
}

.gui_example_source_lines <- function(example_id) {
  specification <- .gui_example_spec(example_id)
  arguments <- c(
    list(scenario = specification$scenario),
    specification$simulation_args
  )
  call <- sprintf(
    "%s = %s", names(arguments), vapply(arguments, .code_value, character(1))
  )
  lines <- c(
    sprintf(
      "simulated <- simulate_som_scenario(%s)",
      paste(call, collapse = ", ")
    ),
    "raw <- as.data.frame(simulated$layers[[1L]])",
    "names(raw) <- sub(\"^environment_\", \"indicator_\", names(raw))",
    "raw$sample_id <- simulated$metadata$id"
  )
  if ("group" %in% specification$metadata) {
    lines <- c(lines, "raw$sampling_group <- simulated$metadata$group")
  }
  if ("domain" %in% specification$metadata) {
    lines <- c(lines, "raw$reporting_region <- simulated$metadata$domain")
  }
  if ("external_label" %in% specification$metadata) {
    lines <- c(
      lines,
      "raw$external_label <- simulated$metadata$external_label"
    )
  }
  lines
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
        "The GUI cannot currently import it to restore controls; ",
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
    .gui_example_source_lines(config$example_id %||% "clusters")
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
      "# The GUI cannot currently import this snapshot to restore controls; ",
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
#' teaching and exploratory configuration. Four guided simulations illustrate
#' discrete classes, a continuous gradient, grouped sampling and transfer
#' across monitoring domains. Recommended settings, downloadable example data,
#' a generic CSV template and in-app interpretation guidance support first-time
#' use. Every completed run can export its exact R script and a YAML
#' configuration snapshot. The GUI cannot currently import that snapshot to
#' restore its controls. The exported script, not the GUI session or snapshot,
#' is the executable analysis record. The interface is designed for a local R
#' session, and `SOMevidence` sends no telemetry. In a local session, selected
#' files remain on the local computer. A remotely deployed Shiny application
#' transfers selected files to its host, whose operator is responsible for
#' access controls and data handling.
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

  splitter_script <- paste0(
    "function initSOMevidenceSplitter() { ",
    "var root = document.querySelector('.somevidence-split-layout > .row'); ",
    "if (!root || root.querySelector('.somevidence-splitter')) return; ",
    "var side = root.querySelector(':scope > .col-sm-4'); ",
    "var main = root.querySelector(':scope > .col-sm-8'); ",
    "if (!side || !main) return; ",
    "side.classList.add('somevidence-sidebar'); ",
    "main.classList.add('somevidence-main'); ",
    "var handle = document.createElement('div'); ",
    "handle.className = 'somevidence-splitter'; ",
    "handle.setAttribute('role', 'separator'); ",
    "handle.setAttribute('aria-orientation', 'vertical'); ",
    "handle.setAttribute('tabindex', '0'); ",
    "handle.dataset.somevidenceTip = 'splitter'; ",
    "handle.dataset.somevidenceTooltip = 'Drag to resize the panels. ",
    "Use arrow keys for small steps; double-click to reset.'; ",
    "var saved = parseFloat(localStorage.getItem('somevidence-sidebar-pct')); ",
    "function setWidth(pct) { pct = Math.max(22, Math.min(55, pct)); ",
    "side.style.flexBasis = pct + '%'; ",
    "handle.setAttribute('aria-valuenow', Math.round(pct)); ",
    "localStorage.setItem('somevidence-sidebar-pct', pct); } ",
    "setWidth(Number.isFinite(saved) ? saved : 33.333); ",
    "root.insertBefore(handle, main); ",
    "var startX = 0, startPct = 0; ",
    "handle.addEventListener('pointerdown', function(event) { ",
    "startX = event.clientX; startPct = side.getBoundingClientRect().width / ",
    "root.getBoundingClientRect().width * 100; ",
    "handle.setPointerCapture(event.pointerId); ",
    "document.body.classList.add('somevidence-resizing'); }); ",
    "handle.addEventListener('pointermove', function(event) { ",
    "if (!handle.hasPointerCapture(event.pointerId)) return; ",
    "var delta = (event.clientX - startX) / ",
    "root.getBoundingClientRect().width * 100; setWidth(startPct + delta); }); ",
    "function stop(event) { if (handle.hasPointerCapture(event.pointerId)) ",
    "handle.releasePointerCapture(event.pointerId); ",
    "document.body.classList.remove('somevidence-resizing'); } ",
    "handle.addEventListener('pointerup', stop); ",
    "handle.addEventListener('pointercancel', stop); ",
    "handle.addEventListener('dblclick', function() { setWidth(33.333); }); ",
    "handle.addEventListener('keydown', function(event) { ",
    "var current = side.getBoundingClientRect().width / ",
    "root.getBoundingClientRect().width * 100; ",
    "if (event.key === 'ArrowLeft') { setWidth(current - 2); ",
    "event.preventDefault(); } ",
    "if (event.key === 'ArrowRight') { setWidth(current + 2); ",
    "event.preventDefault(); } ",
    "if (event.key === 'Home') { setWidth(22); event.preventDefault(); } ",
    "if (event.key === 'End') { setWidth(55); event.preventDefault(); } }); ",
    "} ",
    "document.addEventListener('DOMContentLoaded', initSOMevidenceSplitter); ",
    "document.addEventListener('shiny:connected', initSOMevidenceSplitter);"
  )

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$title(.gui_tr("en", "app_title")),
      shiny::tags$style(shiny::HTML(paste0(
        ".somevidence-table-scroll { overflow-x: auto; width: 100%; } ",
        ".somevidence-table-scroll table { white-space: nowrap; } ",
        ".somevidence-section { color: #1f4e5f; font-weight: 600; ",
        "margin-top: 1.2em; } ",
        ".somevidence-help-card { background: #f5f8f9; ",
        "border-left: 4px solid #2f6f7e; padding: 0.7em 0.9em; ",
        "margin: 0.5em 0 1em 0; } ",
        ".somevidence-help-card p:last-child { margin-bottom: 0; } ",
        ".somevidence-boundary { background: #fff8e8; ",
        "border-left-color: #b7791f; } ",
        ".somevidence-subtitle { color: #555; margin: -0.4em 0 1.2em 0; } ",
        "[data-somevidence-tip] { text-decoration: underline dotted; ",
        "text-underline-offset: 2px; cursor: pointer; } ",
        ".somevidence-tooltip { position: fixed; z-index: 10000; ",
        "max-width: min(340px, calc(100vw - 24px)); padding: 8px 10px; ",
        "border-radius: 5px; color: #fff; background: #243b47; ",
        "font-size: 13px; line-height: 1.45; box-shadow: 0 4px 14px ",
        "rgba(0,0,0,.22); pointer-events: none; opacity: 0; ",
        "transform: translateY(3px); transition: opacity .12s ease, ",
        "transform .12s ease; white-space: normal; } ",
        ".somevidence-tooltip.is-visible { opacity: 1; ",
        "transform: translateY(0); } ",
        ".somevidence-split-layout > .row { display: flex; ",
        "align-items: flex-start; margin-left: 0; margin-right: 0; } ",
        ".somevidence-sidebar { width: auto !important; flex: 0 0 33.333%; ",
        "min-width: 0; } ",
        ".somevidence-main { width: auto !important; flex: 1 1 0; ",
        "min-width: 0; } ",
        ".somevidence-splitter { flex: 0 0 10px; align-self: stretch; ",
        "min-height: 72vh; cursor: col-resize; position: relative; ",
        "outline: none; } ",
        ".somevidence-splitter::before { content: ''; position: absolute; ",
        "left: 4px; top: 0; bottom: 0; width: 2px; background: #c7d2d7; ",
        "border-radius: 2px; } ",
        ".somevidence-splitter:hover::before, ",
        ".somevidence-splitter:focus::before { background: #2f6f7e; ",
        "width: 4px; left: 3px; } ",
        ".somevidence-resizing { cursor: col-resize !important; ",
        "user-select: none; } ",
        "@media (max-width: 767px) { ",
        ".somevidence-split-layout > .row { display: block; } ",
        ".somevidence-sidebar, .somevidence-main { width: 100% !important; ",
        "flex-basis: auto !important; } ",
        ".somevidence-splitter { display: none; } } ",
        ".somevidence-footer { color: #666; font-size: 0.9em; margin-top: 1em; }"
      ))),
      shiny::tags$script(shiny::HTML(splitter_script)),
      shiny::tags$script(shiny::HTML(paste0(
        "Shiny.addCustomMessageHandler('somevidence-language', ",
        "function(message) { ",
        "document.documentElement.lang = message.language === 'zh' ? ",
        "'zh-CN' : 'en'; ",
        "document.title = message.translations.app_title; ",
        "document.querySelectorAll('[data-somevidence-i18n]').forEach(",
        "function(node) { var key = node.dataset.somevidenceI18n; ",
        "if (message.translations[key]) node.textContent = ",
        "message.translations[key]; }); ",
        "document.querySelectorAll('[data-somevidence-tip]').forEach(",
        "function(node) { var key = node.dataset.somevidenceTip; ",
        "if (message.tooltips[key]) node.dataset.somevidenceTooltip = ",
        "message.tooltips[key]; var label = node.closest('label'); ",
        "if (label && label !== node && message.tooltips[key]) { ",
        "label.dataset.somevidenceTip = key; ",
        "label.dataset.somevidenceTooltip = message.tooltips[key]; ",
        "label.setAttribute('tabindex', '0'); } }); ",
        "var tabs = {results: 'tab_results', interpretation: ",
        "'tab_interpretation', examples: 'tab_examples'}; ",
        "Object.keys(tabs).forEach(function(value) { var node = ",
        "document.querySelector('a[data-value=\"' + value + '\"]'); ",
        "if (node) node.textContent = message.translations[tabs[value]]; }); ",
        "function applyTermTips() { document.querySelectorAll('th,td').",
        "forEach(function(node) { var text = node.textContent.trim(); ",
        "if (message.term_tips[text]) { node.dataset.somevidenceTooltip = ",
        "message.term_tips[text]; node.setAttribute('tabindex', '0'); ",
        "node.style.cursor = 'pointer'; } }); } ",
        "window.somevidenceApplyTermTips = applyTermTips; ",
        "if (!window.somevidenceTooltipObserver) { ",
        "window.somevidenceTooltipObserver = new MutationObserver(",
        "function() { if (window.somevidenceApplyTermTips) ",
        "window.somevidenceApplyTermTips(); }); ",
        "window.somevidenceTooltipObserver.observe(document.body, ",
        "{childList: true, subtree: true}); } ",
        "applyTermTips(); window.setTimeout(applyTermTips, 250); ",
        "});"
      ))),
      shiny::tags$script(shiny::HTML(paste0(
        "(function() { ",
        "function tooltip() { var tip = document.querySelector(",
        "'.somevidence-tooltip'); if (!tip) { tip = ",
        "document.createElement('div'); tip.className = ",
        "'somevidence-tooltip'; tip.setAttribute('role', 'tooltip'); ",
        "document.body.appendChild(tip); } return tip; } ",
        "function show(target) { var text = target.dataset.",
        "somevidenceTooltip; if (!text) return; var tip = tooltip(); ",
        "tip.textContent = text; tip.classList.add('is-visible'); ",
        "var rect = target.getBoundingClientRect(); var box = ",
        "tip.getBoundingClientRect(); var left = Math.min(",
        "window.innerWidth - box.width - 12, Math.max(12, rect.left)); ",
        "var top = rect.bottom + 8; if (top + box.height > ",
        "window.innerHeight - 12) top = Math.max(12, rect.top - ",
        "box.height - 8); tip.style.left = left + 'px'; ",
        "tip.style.top = top + 'px'; target.setAttribute(",
        "'aria-describedby', 'somevidence-active-tooltip'); ",
        "tip.id = 'somevidence-active-tooltip'; } ",
        "function hide(target) { var tip = document.querySelector(",
        "'.somevidence-tooltip'); if (tip) tip.classList.remove(",
        "'is-visible'); if (target) target.removeAttribute(",
        "'aria-describedby'); } ",
        "function targetFor(node) { if (!node || !node.closest) return null; ",
        "var target = node.closest('[data-somevidence-tooltip]'); ",
        "if (target) return target; var label = node.closest('label'); ",
        "return label ? label.querySelector('[data-somevidence-tooltip]') : ",
        "null; } ",
        "document.addEventListener('mouseover', function(event) { ",
        "var target = targetFor(event.target); ",
        "if (target) show(target); }); ",
        "document.addEventListener('mouseout', function(event) { ",
        "var target = targetFor(event.target); ",
        "if (target && !target.contains(event.relatedTarget)) hide(target); }); ",
        "document.addEventListener('focusin', function(event) { ",
        "var target = targetFor(event.target); ",
        "if (target) show(target); }); ",
        "document.addEventListener('focusout', function(event) { ",
        "var target = targetFor(event.target); ",
        "if (target) hide(target); }); ",
        "document.addEventListener('keydown', function(event) { ",
        "if (event.key === 'Escape') hide(document.activeElement); }); ",
        "})();"
      )))
    ),
    shiny::fluidRow(
      shiny::column(9, shiny::tags$h2(.gui_i18n("app_title"))),
      shiny::column(
        3,
        shiny::selectInput(
          "language", "Language / \u8bed\u8a00",
          choices = c("English" = "en", "\u4e2d\u6587" = "zh"),
          selected = "en", width = "100%"
        )
      )
    ),
    shiny::p(
      class = "somevidence-subtitle",
      .gui_i18n("app_subtitle")
    ),
    # nolint start: indentation_linter
    shiny::div(class = "somevidence-split-layout",
               shiny::sidebarLayout(
                 shiny::sidebarPanel(
        shiny::div(class = "somevidence-section", .gui_i18n("section_data")),
        shiny::radioButtons(
          "data_source", .gui_i18n("data_source"),
          choices = c(
            "Built-in simulation" = "built_in", "Upload CSV" = "upload"
          )
        ),
        shiny::conditionalPanel(
          "input.data_source == 'built_in'",
          shiny::selectInput(
            "example_id", .gui_i18n("guided_example"),
            choices = .gui_example_choices(), selected = "clusters"
          ),
          shiny::uiOutput("example_summary"),
          shiny::actionButton(
            "reset_example", .gui_i18n("reset_example"),
            width = "100%"
          ),
          shiny::downloadButton(
            "download_example", .gui_i18n("download_example")
          )
        ),
        shiny::conditionalPanel(
          "input.data_source == 'upload'",
          shiny::fileInput("file", .gui_i18n("csv_file"), accept = ".csv"),
          shiny::downloadButton(
            "download_template", .gui_i18n("download_template")
          ),
          shiny::helpText(.gui_i18n("upload_help"))
        ),
        shiny::selectizeInput(
          "predictors", .gui_i18n("predictors"), choices = NULL,
          multiple = TRUE
        ),
        shiny::conditionalPanel(
          "input.data_source == 'built_in'",
          shiny::helpText(.gui_i18n("simulated_predictor_note"))
        ),
        shiny::selectInput(
          "id_column", .gui_i18n("sample_id"), choices = "None"
        ),
        shiny::selectInput(
          "group_column", .gui_i18n("sampling_group"), choices = "None"
        ),
        shiny::selectInput(
          "time_column", .gui_i18n("sampling_time"), choices = "None"
        ),
        shiny::selectInput(
          "domain_column", .gui_i18n("transfer_domain"), choices = "None"
        ),
        shiny::selectInput(
          "weight_column", .gui_i18n("weight"),
          choices = "None"
        ),
        shiny::selectInput(
          "external_column", .gui_i18n("external_label"), choices = "None"
        ),
        shiny::helpText(.gui_i18n("metadata_help")),
        shiny::div(
          class = "somevidence-section", .gui_i18n("section_preprocess")
        ),
        shiny::selectInput(
          "transform", .gui_i18n("transformation"),
          choices = c("identity", "log", "log1p", "sqrt", "hellinger", "clr")
        ),
        shiny::checkboxInput("center", .gui_i18n("center"), TRUE),
        shiny::checkboxInput("scale", .gui_i18n("scale"), TRUE),
        shiny::conditionalPanel(
          "input.transform == 'clr'",
          shiny::numericInput(
            "zero_replacement", .gui_i18n("zero_replacement"), 1e-6,
            min = .Machine$double.eps
          )
        ),
        shiny::helpText(.gui_i18n("preprocess_help")),
        shiny::div(
          class = "somevidence-section", .gui_i18n("section_resampling")
        ),
        shiny::selectInput(
          "resample_method", .gui_i18n("resampling"),
          choices = c(
            "full", "subsample", "group_subsample", "leave_domain_out"
          )
        ),
        shiny::conditionalPanel(
          paste0(
            "input.resample_method == 'subsample' || ",
            "input.resample_method == 'group_subsample'"
          ),
          shiny::numericInput(
            "repeats", .gui_i18n("repeats"), 5L, min = 1L
          ),
          shiny::numericInput(
            "prop", .gui_i18n("proportion"), 0.8, min = 0.1, max = 1
          )
        ),
        shiny::helpText(.gui_i18n("resampling_help")),
        shiny::div(
          class = "somevidence-section", .gui_i18n("section_ensemble")
        ),
        shiny::numericInput("xdim", .gui_i18n("grid_width"), 7L, min = 2L),
        shiny::numericInput("ydim", .gui_i18n("grid_height"), 5L, min = 2L),
        shiny::textInput("seeds", .gui_i18n("som_seeds"), "1,2,3"),
        shiny::numericInput(
          "rlen", .gui_i18n("training_iterations"), 500L, min = 10L
        ),
        shiny::textInput("k", .gui_i18n("candidate_k"), "2,3,4,5"),
        shiny::div(
          class = "somevidence-section", .gui_i18n("section_export")
        ),
        shiny::checkboxGroupInput(
          "cross_models", .gui_i18n("cross_models"),
          choices = c("K-means" = "kmeans", "Ward.D2" = "ward", "GMM" = "gmm"),
          selected = c("kmeans", "ward")
        ),
        shiny::actionButton("run", .gui_i18n("run_workflow")),
        shiny::downloadButton("download_r", .gui_i18n("export_r")),
        shiny::downloadButton(
          "download_yaml", .gui_i18n("export_yaml")
        ),
        shiny::helpText(.gui_i18n("export_help"))
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Run and results", value = "results",
            shiny::h4(.gui_i18n("data_preflight")),
            shiny::p(.gui_i18n("preflight_prompt")),
            shiny::verbatimTextOutput("preflight_status"),
            shiny::div(
              class = "somevidence-table-scroll",
              shiny::tableOutput("data_audit")
            ),
            shiny::hr(),
            shiny::radioButtons(
              "plot_type", .gui_i18n("evidence_view"),
              choices = c(
                "Representation" = "audit",
                "Partition stability" = "partitions",
                "Consensus" = "consensus",
                "Cross-model agreement" = "cross_model"
              )
            ),
            shiny::uiOutput("view_guidance"),
            shiny::conditionalPanel(
              "input.plot_type == 'consensus'",
              shiny::selectInput(
                "consensus_k", .gui_i18n("consensus_solution"),
                choices = character()
              )
            ),
            shiny::verbatimTextOutput("status"),
            shiny::plotOutput("evidence_plot", height = "520px"),
            shiny::h4(.gui_i18n("run_diagnostics")),
            shiny::div(
              class = "somevidence-table-scroll",
              shiny::tableOutput("diagnostics_table")
            ),
            shiny::h4(.gui_i18n("partition_stability")),
            shiny::div(
              class = "somevidence-table-scroll",
              shiny::tableOutput("partition_table")
            ),
            shiny::h4(.gui_i18n("cross_model_agreement")),
            shiny::div(
              class = "somevidence-table-scroll",
              shiny::tableOutput("cross_table")
            )
          ),
          shiny::tabPanel(
            "Interpretation guide", value = "interpretation",
            shiny::h3(.gui_i18n("interpretation_heading")),
            shiny::p(.gui_i18n("interpretation_intro")),
            shiny::div(
              class = "somevidence-table-scroll",
              shiny::tableOutput("metric_guide")
            ),
            shiny::h3(.gui_i18n("preflight_messages")),
            shiny::div(
              class = "somevidence-table-scroll",
              shiny::tableOutput("troubleshooting_guide")
            ),
            shiny::div(
              class = "somevidence-help-card somevidence-boundary",
              shiny::tags$strong(.gui_i18n("reporting_boundary")),
              shiny::tags$p(.gui_i18n("reporting_text"))
            )
          ),
          shiny::tabPanel(
            "Examples and help", value = "examples",
            shiny::h3(.gui_i18n("examples_heading")),
            shiny::p(.gui_i18n("examples_intro")),
            shiny::uiOutput("example_details"),
            shiny::div(
              class = "somevidence-table-scroll",
              shiny::tableOutput("example_table")
            ),
            shiny::h3(.gui_i18n("workflow_heading")),
            shiny::tags$ol(
              shiny::tags$li(.gui_i18n("workflow_step_1")),
              shiny::tags$li(.gui_i18n("workflow_step_2")),
              shiny::tags$li(.gui_i18n("workflow_step_3")),
              shiny::tags$li(.gui_i18n("workflow_step_4")),
              shiny::tags$li(.gui_i18n("workflow_step_5"))
            ),
            shiny::tags$p(
              shiny::tags$a(
                .gui_i18n("guide_link"),
                href = paste0(
                  "https://shaowen-ye.github.io/SOMevidence/",
                  "articles/v02-shiny-interface.html"
                ),
                target = "_blank", rel = "noopener noreferrer"
              )
            )
          )
        )
      )
    )),
    # nolint end
    shiny::tags$footer(
      class = "somevidence-footer",
      shiny::tags$hr(),
      shiny::tags$p(
        .gui_i18n("footer"),
        " | SOMevidence ",
        as.character(utils::packageVersion("SOMevidence"))
      )
    )
  )

  server <- function(input, output, session) {
    current_language <- shiny::reactive(.gui_language(input$language))

    shiny::observeEvent(input$language, {
      language <- current_language()
      session$sendCustomMessage(
        "somevidence-language",
        list(
          language = language,
          translations = as.list(.gui_translations()[[language]]),
          tooltips = as.list(.gui_tooltips()[[language]]),
          term_tips = as.list(.gui_table_term_tooltips(language))
        )
      )
      shiny::updateRadioButtons(
        session, "data_source",
        choices = if (language == "zh") {
          c("\u5185\u7f6e\u6a21\u62df\u6570\u636e" = "built_in", "\u4e0a\u4f20 CSV" = "upload")
        } else {
          c("Built-in simulation" = "built_in", "Upload CSV" = "upload")
        },
        selected = input$data_source %||% "built_in"
      )
      shiny::updateSelectInput(
        session, "example_id",
        choices = .gui_example_choices(language),
        selected = input$example_id %||% "clusters"
      )
      shiny::updateSelectInput(
        session, "resample_method",
        choices = if (language == "zh") {
          c(
            "\u4ec5\u5168\u6570\u636e\u91cd\u590d\u8bad\u7ec3" = "full",
            "\u6309\u884c\u91cd\u62bd\u6837" = "subsample",
            "\u6309\u5b8c\u6574\u5206\u7ec4\u91cd\u62bd\u6837" = "group_subsample",
            "\u6bcf\u6b21\u7559\u51fa\u4e00\u4e2a\u76d1\u6d4b\u57df" = "leave_domain_out"
          )
        } else {
          c(
            "Full data" = "full", "Row subsampling" = "subsample",
            "Whole-group subsampling" = "group_subsample",
            "Leave one domain out" = "leave_domain_out"
          )
        },
        selected = input$resample_method %||% "subsample"
      )
      shiny::updateRadioButtons(
        session, "plot_type",
        choices = if (language == "zh") {
          c(
            "\u8868\u5f81\u8d28\u91cf" = "audit", "\u5206\u7ec4\u7a33\u5b9a\u6027" = "partitions",
            "\u6837\u672c\u5171\u8bc6" = "consensus", "\u8de8\u6a21\u578b\u4e00\u81f4\u6027" = "cross_model"
          )
        } else {
          c(
            "Representation" = "audit",
            "Partition stability" = "partitions",
            "Consensus" = "consensus",
            "Cross-model agreement" = "cross_model"
          )
        },
        selected = input$plot_type %||% "audit"
      )
    }, ignoreInit = FALSE)

    raw_data <- shiny::reactive({
      if (input$data_source == "built_in") {
        return(.gui_builtin_example(input$example_id %||% "clusters"))
      }
      shiny::req(input$file)
      .read_gui_csv(input$file$datapath)
    })

    apply_example_defaults <- function(example_id) {
      defaults <- .gui_example_defaults(example_id)
      shiny::updateSelectInput(
        session, "transform", selected = defaults$transform
      )
      shiny::updateCheckboxInput(
        session, "center", value = defaults$center
      )
      shiny::updateCheckboxInput(session, "scale", value = defaults$scale)
      shiny::updateSelectInput(
        session, "resample_method", selected = defaults$resample_method
      )
      shiny::updateNumericInput(
        session, "repeats", value = defaults$repeats
      )
      shiny::updateNumericInput(session, "prop", value = defaults$prop)
      shiny::updateNumericInput(session, "xdim", value = defaults$xdim)
      shiny::updateNumericInput(session, "ydim", value = defaults$ydim)
      shiny::updateTextInput(
        session, "seeds", value = paste(defaults$seeds, collapse = ",")
      )
      shiny::updateNumericInput(session, "rlen", value = defaults$rlen)
      shiny::updateTextInput(
        session, "k", value = paste(defaults$k, collapse = ",")
      )
      shiny::updateCheckboxGroupInput(
        session, "cross_models", selected = defaults$cross_models
      )
    }

    shiny::observeEvent(input$example_id, {
      if (identical(input$data_source, "built_in")) {
        apply_example_defaults(input$example_id %||% "clusters")
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$reset_example, {
      shiny::req(identical(input$data_source, "built_in"))
      apply_example_defaults(input$example_id %||% "clusters")
    })

    shiny::observeEvent(list(raw_data(), input$language), {
      raw <- raw_data()
      numeric_columns <- names(raw)[vapply(raw, is.numeric, logical(1))]
      metadata_defaults <- .gui_metadata_defaults(names(raw))
      default_predictors <- .gui_predictor_defaults(raw, input$data_source)
      retained_predictors <- intersect(
        input$predictors %||% character(), numeric_columns
      )
      selected_predictors <- if (length(retained_predictors)) {
        retained_predictors
      } else {
        default_predictors
      }
      none_label <- if (current_language() == "zh") "\u65e0" else "None"
      metadata_choices <- stats::setNames(
        c("None", names(raw)), c(none_label, names(raw))
      )
      shiny::updateSelectizeInput(
        session, "predictors",
        choices = numeric_columns, selected = selected_predictors,
        server = TRUE
      )
      selected_metadata <- vapply(names(metadata_defaults), function(id) {
        current <- input[[id]] %||% ""
        if (current %in% c("None", names(raw))) {
          current
        } else {
          metadata_defaults[[id]]
        }
      }, character(1))
      shiny::updateSelectInput(
        session, "id_column",
        choices = metadata_choices,
        selected = selected_metadata[["id_column"]]
      )
      for (id in setdiff(names(metadata_defaults), "id_column")) {
        shiny::updateSelectInput(
          session, id,
          choices = metadata_choices, selected = selected_metadata[[id]]
        )
      }
    }, ignoreInit = FALSE)

    as_column <- function(value) if (identical(value, "None")) NULL else value
    gui_config <- shiny::reactive({
      list(
        schema_version = 1L,
        package_version = as.character(utils::packageVersion("SOMevidence")),
        data_source = input$data_source,
        example_id = if (input$data_source == "built_in") {
          input$example_id %||% "clusters"
        } else {
          NULL
        },
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
      audit <- .gui_data_audit(raw_data(), input$predictors)
      .gui_localize_data_audit(audit, current_language())
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$preflight_status <- shiny::renderText({
      .format_gui_preflight_status(preflight(), current_language())
    })
    output$example_summary <- shiny::renderUI({
      specification <- .gui_example_spec(input$example_id %||% "clusters")
      shiny::div(
        class = "somevidence-help-card",
        shiny::tags$strong(.gui_example_value(
          specification, "label", current_language()
        )),
        shiny::tags$p(.gui_example_value(
          specification, "question", current_language()
        ))
      )
    })
    output$example_details <- shiny::renderUI({
      specification <- .gui_example_spec(input$example_id %||% "clusters")
      language <- current_language()
      shiny::tagList(
        shiny::div(
          class = "somevidence-help-card",
          shiny::tags$strong(.gui_example_value(
            specification, "label", language
          )),
          shiny::tags$p(
            shiny::tags$b(.gui_tr(language, "question")),
            .gui_example_value(specification, "question", language)
          ),
          shiny::tags$p(
            shiny::tags$b(.gui_tr(language, "design")),
            .gui_example_value(specification, "design", language)
          ),
          shiny::tags$p(
            shiny::tags$b(.gui_tr(language, "learning_goal")),
            .gui_example_value(specification, "lesson", language)
          )
        ),
        shiny::div(
          class = "somevidence-help-card somevidence-boundary",
          shiny::tags$strong(.gui_tr(language, "interpretive_boundary")),
          shiny::tags$p(.gui_example_value(
            specification, "boundary", language
          ))
        )
      )
    })
    output$view_guidance <- shiny::renderUI({
      language <- current_language()
      guidance <- .gui_view_guidance(
        input$plot_type %||% "audit", language
      )
      shiny::div(
        class = "somevidence-help-card",
        shiny::tags$strong(guidance$title),
        shiny::tags$p(
          shiny::tags$b(.gui_tr(language, "question")), guidance$question
        ),
        shiny::tags$p(
          shiny::tags$b(.gui_tr(language, "inspect")), guidance$inspect
        ),
        shiny::tags$p(
          shiny::tags$b(.gui_tr(language, "do_not_infer")), guidance$boundary
        )
      )
    })
    output$example_table <- shiny::renderTable({
      .gui_example_table(current_language())
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$metric_guide <- shiny::renderTable({
      .gui_metric_guide(current_language())
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$troubleshooting_guide <- shiny::renderTable({
      .gui_troubleshooting_guide(current_language())
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)

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
      .gui_workflow_status(analysis()$workflow, current_language())
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
      .gui_workflow_diagnostics(analysis()$workflow, current_language())
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$partition_table <- shiny::renderTable({
      .gui_table_labels(
        analysis()$workflow$partitions$stability,
        current_language()
      )
    }, digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$cross_table <- shiny::renderTable({
      comparison <- analysis()$workflow$cross_comparison
      if (is.null(comparison)) {
        return(data.frame(Note = "Not requested", check.names = FALSE))
      }
      .gui_table_labels(comparison$summary, current_language())
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
    output$download_example <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "somevidence_example_",
          input$example_id %||% "clusters",
          ".csv"
        )
      },
      content = function(file) {
        utils::write.csv(
          .gui_builtin_example(input$example_id %||% "clusters"),
          file, row.names = FALSE, na = ""
        )
      }
    )
    output$download_template <- shiny::downloadHandler(
      filename = function() "somevidence_csv_template.csv",
      content = function(file) {
        utils::write.csv(.gui_csv_template(), file, row.names = FALSE, na = "")
      }
    )
  }
  shiny::shinyApp(ui, server)
}
