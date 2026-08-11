#!/usr/bin/env Rscript

# Reproduce directional local benchmarks for the computation-equivalent
# changes introduced in SOMevidence 1.1.3. These timings are descriptive, not
# cross-platform acceptance thresholds. Every timed comparison first requires
# identical outputs from the version 1.1.2 reference kernel and current code.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else if (requireNamespace("SOMevidence", quietly = TRUE)) {
  if (utils::packageVersion("SOMevidence") != "1.1.3") {
    stop(
      "Install SOMevidence 1.1.3 or run from a source library with `pkgload`.",
      call. = FALSE
    )
  }
} else {
  stop(
    "Install SOMevidence 1.1.3 or install `pkgload` to run from source.",
    call. = FALSE
  )
}

oracle_environment <- new.env(parent = globalenv())
sys.source(
  "tests/testthat/helper-v113-consensus-oracles.R",
  envir = oracle_environment
)
sys.source(
  "tests/testthat/helper-v113-agreement-oracles.R",
  envir = oracle_environment
)

time_function <- function(fun, repetitions = 3L) {
  force(fun)
  timings <- numeric(repetitions)
  for (i in seq_len(repetitions)) {
    gc(FALSE)
    timings[[i]] <- system.time(fun())[["elapsed"]]
  }
  stats::median(timings)
}

benchmark_row <- function(operation, before, after, unit = "seconds") {
  data.frame(
    operation = operation,
    v1.1.2 = before,
    v1.1.3 = after,
    ratio = before / after,
    unit = unit,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

set.seed(11301L)

# Compact ensemble tasks ------------------------------------------------------
n_splits <- 100L
n_budget <- 500L
splits <- lapply(seq_len(n_splits), function(i) {
  list(
    id = paste0("split", i),
    analysis = seq_len(400L),
    assessment = integer()
  )
})
budget <- data.frame(
  model_id = paste0("model", seq_len(n_budget)),
  grid_id = rep(seq_len(5L), length.out = n_budget),
  xdim = rep(5:9, length.out = n_budget),
  ydim = rep(4:8, length.out = n_budget),
  seed = seq_len(n_budget),
  stringsAsFactors = FALSE
)
old_jobs <- vector("list", n_splits * n_budget)
cursor <- 0L
for (split in splits) {
  for (i in seq_len(nrow(budget))) {
    cursor <- cursor + 1L
    job <- budget[i, , drop = FALSE]
    old_jobs[[cursor]] <- list(
      split = split,
      job = job,
      fit_id = paste(split$id, job$model_id, sep = "__")
    )
  }
}
compact_jobs <- seq_len(n_splits * n_budget)
task_sizes <- benchmark_row(
  "ensemble task metadata",
  as.numeric(utils::object.size(old_jobs)) / 1024^2,
  as.numeric(utils::object.size(compact_jobs)) / 1024^2,
  "MiB"
)
rm(old_jobs, compact_jobs)

# Analysis-row distance preparation -----------------------------------------
n_all <- 10000L
n_analysis <- 1000L
n_units <- 48L
n_variables <- 9L
fit <- list(
  success = TRUE,
  processed_all = list(matrix(
    stats::rnorm(n_all * n_variables),
    nrow = n_all,
    ncol = n_variables
  )),
  codes = list(matrix(
    stats::rnorm(n_units * n_variables),
    nrow = n_units,
    ncol = n_variables
  )),
  user_weights = 1,
  distance_weights = 1
)
analysis_rows <- seq_len(n_analysis)
old_distance <- oracle_environment$.v112_model_distance_matrix(fit)[
  analysis_rows,
  ,
  drop = FALSE
]
new_distance <- SOMevidence:::.model_distance_matrix(fit, analysis_rows)
stopifnot(identical(old_distance, new_distance))
distance_row <- benchmark_row(
  "analysis-row distance matrix",
  time_function(function() {
    oracle_environment$.v112_model_distance_matrix(fit)[
      analysis_rows,
      ,
      drop = FALSE
    ]
  }),
  time_function(function() {
    SOMevidence:::.model_distance_matrix(fit, analysis_rows)
  })
)
rm(old_distance, new_distance, fit)

# Complete-data co-assignment -----------------------------------------------
n_samples <- 1500L
n_partitions <- 20L
records <- lapply(seq_len(n_partitions), function(i) {
  labels <- sample.int(3L, n_samples, replace = TRUE)
  names(labels) <- paste0("sample", seq_len(n_samples))
  list(id = paste0("fit", i), sample_labels = labels)
})
old_coassignment <- oracle_environment$.v112_coassignment(records)
new_coassignment <- SOMevidence:::.complete_coassignment(records, n_samples)
diag(new_coassignment) <- 1
stopifnot(identical(old_coassignment, new_coassignment))
coassignment_row <- benchmark_row(
  "complete-data co-assignment",
  time_function(function() oracle_environment$.v112_coassignment(records)),
  time_function(function() {
    output <- SOMevidence:::.complete_coassignment(records, n_samples)
    diag(output) <- 1
    output
  })
)
rm(old_coassignment, new_coassignment)

# Incremental alignment propagation -----------------------------------------
propagation_records <- lapply(seq_len(50L), function(i) {
  labels <- sample.int(3L, 400L, replace = TRUE)
  list(id = paste0("fit", i), sample_labels = labels)
})
old_propagation <- oracle_environment$.v112_propagate_alignment(
  propagation_records,
  reference_index = 1L,
  k = 3L
)
new_propagation <- SOMevidence:::.propagate_alignment(
  propagation_records,
  reference_index = 1L,
  k = 3L
)
stopifnot(identical(old_propagation, new_propagation))
propagation_row <- benchmark_row(
  "aligned-vote propagation",
  time_function(function() {
    oracle_environment$.v112_propagate_alignment(
      propagation_records,
      reference_index = 1L,
      k = 3L
    )
  }),
  time_function(function() {
    SOMevidence:::.propagate_alignment(
      propagation_records,
      reference_index = 1L,
      k = 3L
    )
  })
)
rm(old_propagation, new_propagation)

# Ward.D2 tree reuse ----------------------------------------------------------
ward_matrix <- matrix(stats::rnorm(1000L * 10L), nrow = 1000L, ncol = 10L)
candidate_k <- 2:8
analysis <- seq_len(nrow(ward_matrix))
old_ward <- function() {
  lapply(candidate_k, function(k) {
    SOMevidence:::.fit_cross_partition(
      ward_matrix, analysis, "ward", k, NA_integer_, 50L, 100L, NULL
    )
  })
}
new_ward <- function() {
  tree <- SOMevidence:::.fit_ward_tree(ward_matrix)
  lapply(candidate_k, function(k) {
    SOMevidence:::.fit_cross_partition(
      ward_matrix, analysis, "ward", k, NA_integer_, 50L, 100L, NULL,
      ward_tree = tree
    )
  })
}
old_ward_output <- old_ward()
new_ward_output <- new_ward()
stopifnot(identical(old_ward_output, new_ward_output))
ward_row <- benchmark_row(
  "Ward.D2 tree across k = 2:8",
  time_function(old_ward),
  time_function(new_ward)
)

results <- rbind(
  task_sizes,
  distance_row,
  coassignment_row,
  propagation_row,
  ward_row
)

cat("SOMevidence ", as.character(utils::packageVersion("SOMevidence")), "\n", sep = "")
cat(R.version.string, "\n")
cat("Platform: ", R.version$platform, "\n\n", sep = "")
cat("| Operation | v1.1.2 | v1.1.3 | Before/after | Unit |\n")
cat("|---|---:|---:|---:|---|\n")
for (i in seq_len(nrow(results))) {
  cat(sprintf(
    "| %s | %.3f | %.3f | %.2fx | %s |\n",
    results$operation[[i]],
    results$v1.1.2[[i]],
    results$v1.1.3[[i]],
    results$ratio[[i]],
    results$unit[[i]]
  ))
}
