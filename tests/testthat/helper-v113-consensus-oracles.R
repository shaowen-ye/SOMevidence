.v112_coassignment <- function(records) {
  n <- length(records[[1L]]$sample_labels)
  coassignment_sum <- matrix(0, nrow = n, ncol = n)
  coassignment_n <- matrix(0L, nrow = n, ncol = n)
  for (record in records) {
    labels <- record$sample_labels
    observed <- !is.na(labels)
    pair_observed <- outer(observed, observed, "&")
    pair_same <- outer(labels, labels, "==")
    pair_same[is.na(pair_same)] <- FALSE
    coassignment_sum <- coassignment_sum + pair_same
    coassignment_n <- coassignment_n + pair_observed
  }
  if (any(coassignment_n == 0L)) {
    SOMevidence:::.abort(
      "Some sample pairs were never jointly assigned across the ensemble."
    )
  }
  coassignment <- coassignment_sum / coassignment_n
  diag(coassignment) <- 1
  coassignment
}

.v112_align_from_parents <- function(labels, parents, k) {
  if (is.null(dim(parents))) {
    parents <- matrix(parents, ncol = 1L)
  }
  repeated_labels <- rep(labels, ncol(parents))
  parent_labels <- as.vector(parents)
  keep <- !is.na(repeated_labels) & !is.na(parent_labels)
  contingency <- table(
    factor(repeated_labels[keep], levels = seq_len(k)),
    factor(parent_labels[keep], levels = seq_len(k))
  )
  assignment <- clue::solve_LSAP(contingency, maximum = TRUE)
  mapping <- as.integer(assignment)
  supported_match <- contingency[cbind(seq_len(k), mapping)] > 0L
  mapping[!supported_match] <- NA_integer_
  output <- rep(NA_integer_, length(labels))
  output[!is.na(labels)] <- mapping[as.integer(labels[!is.na(labels)])]
  parent_support <- colSums(
    !is.na(parents) & !is.na(labels)
  ) > 0L
  list(
    labels = output,
    n_joint = sum(keep),
    parent_support = parent_support
  )
}

.v112_propagate_alignment <- function(records, reference_index, k) {
  n_records <- length(records)
  aligned <- vector("list", n_records)
  aligned[[reference_index]] <- records[[reference_index]]$sample_labels
  reached <- reference_index
  diagnostics <- list()

  while (length(reached) < n_records) {
    candidates <- list()
    cursor <- 0L
    parent_matrix <- do.call(cbind, aligned[reached])
    for (child in setdiff(seq_len(n_records), reached)) {
      child_labels <- records[[child]]$sample_labels
      candidate <- .v112_align_from_parents(
        child_labels, parent_matrix, k
      )
      raw_clusters <- sort(unique(stats::na.omit(child_labels)))
      resolved_clusters <- sort(unique(stats::na.omit(candidate$labels)))
      if (length(resolved_clusters) == length(raw_clusters)) {
        cursor <- cursor + 1L
        candidates[[cursor]] <- list(
          parents = reached[candidate$parent_support],
          child = child,
          n_joint = candidate$n_joint,
          alignment = candidate$labels,
          n_child_clusters = length(raw_clusters),
          n_resolved_clusters = length(resolved_clusters)
        )
      }
    }
    if (!length(candidates)) {
      SOMevidence:::.abort(paste0(
        "Consensus labels could not be propagated through the partition-",
        "overlap graph because an intermediate mapping was unidentifiable."
      ))
    }
    selected <- candidates[[which.max(vapply(
      candidates, `[[`, numeric(1), "n_joint"
    ))]]
    child <- selected$child
    aligned[[child]] <- selected$alignment
    diagnostics[[length(diagnostics) + 1L]] <- data.frame(
      parent_fit = paste(vapply(
        records[selected$parents], `[[`, character(1), "id"
      ), collapse = ";"),
      n_parent_fits = length(selected$parents),
      child_fit = records[[child]]$id,
      n_joint = as.integer(selected$n_joint),
      n_child_clusters = selected$n_child_clusters,
      n_resolved_clusters = selected$n_resolved_clusters,
      stringsAsFactors = FALSE
    )
    reached <- c(reached, child)
  }

  alignment <- do.call(cbind, aligned)
  colnames(alignment) <- vapply(records, `[[`, character(1), "id")
  diagnostic_table <- if (length(diagnostics)) {
    do.call(rbind, diagnostics)
  } else {
    data.frame(
      parent_fit = character(), n_parent_fits = integer(),
      child_fit = character(), n_joint = integer(),
      n_child_clusters = integer(), n_resolved_clusters = integer(),
      stringsAsFactors = FALSE
    )
  }
  list(aligned = alignment, diagnostics = diagnostic_table)
}
