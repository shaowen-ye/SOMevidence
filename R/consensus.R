.align_labels <- function(labels, reference, k) {
  keep <- !is.na(labels) & !is.na(reference)
  contingency <- table(
    factor(labels[keep], levels = seq_len(k)),
    factor(reference[keep], levels = seq_len(k))
  )
  assignment <- clue::solve_LSAP(contingency, maximum = TRUE)
  mapping <- as.integer(assignment)
  supported_match <- contingency[cbind(seq_len(k), mapping)] > 0L
  mapping[!supported_match] <- NA_integer_
  aligned <- rep(NA_integer_, length(labels))
  aligned[!is.na(labels)] <- mapping[as.integer(labels[!is.na(labels)])]
  aligned
}

.align_labels_from_parents <- function(labels, parents, k) {
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

.new_alignment_state <- function(n_records, k) {
  levels <- as.character(seq_len(k))
  list(
    contingency = structure(
      matrix(
        0L,
        nrow = k,
        ncol = k,
        dimnames = list(labels = levels, parents = levels)
      ),
      class = "table"
    ),
    n_joint = 0L,
    parent_support = rep(FALSE, n_records)
  )
}

.add_alignment_parent <- function(
  state, labels, parent_labels, parent_index, k
) {
  keep <- !is.na(labels) & !is.na(parent_labels)
  contribution <- table(
    factor(labels[keep], levels = seq_len(k)),
    factor(parent_labels[keep], levels = seq_len(k))
  )
  state$contingency[] <- state$contingency + contribution
  state$n_joint <- state$n_joint + sum(keep)
  state$parent_support[[parent_index]] <- any(keep)
  state
}

.align_labels_from_state <- function(labels, state, k, reached) {
  assignment <- clue::solve_LSAP(state$contingency, maximum = TRUE)
  mapping <- as.integer(assignment)
  supported_match <- state$contingency[
    cbind(seq_len(k), mapping)
  ] > 0L
  mapping[!supported_match] <- NA_integer_
  output <- rep(NA_integer_, length(labels))
  output[!is.na(labels)] <- mapping[as.integer(labels[!is.na(labels)])]
  list(
    labels = output,
    n_joint = state$n_joint,
    parent_support = state$parent_support[reached]
  )
}

.medoid_partition <- function(records) {
  n_records <- length(records)
  agreement <- matrix(1, nrow = n_records, ncol = n_records)
  if (n_records > 1L) {
    pairs <- utils::combn(seq_len(n_records), 2L)
    for (i in seq_len(ncol(pairs))) {
      a <- pairs[1L, i]
      b <- pairs[2L, i]
      agreement[a, b] <- agreement[b, a] <- .adjusted_rand(
        records[[a]]$sample_labels,
        records[[b]]$sample_labels
      )
    }
  }
  which.max(rowMeans(agreement, na.rm = TRUE))
}

.overlap_graph_connected <- function(records) {
  n_records <- length(records)
  adjacency <- diag(TRUE, n_records)
  if (n_records > 1L) {
    pairs <- utils::combn(seq_len(n_records), 2L)
    for (i in seq_len(ncol(pairs))) {
      a <- pairs[1L, i]
      b <- pairs[2L, i]
      jointly_observed <- !is.na(records[[a]]$sample_labels) &
        !is.na(records[[b]]$sample_labels)
      adjacency[a, b] <- adjacency[b, a] <- any(jointly_observed)
    }
  }
  reached <- 1L
  frontier <- 1L
  while (length(frontier)) {
    neighbours <- which(colSums(adjacency[frontier, , drop = FALSE]) > 0L)
    frontier <- setdiff(neighbours, reached)
    reached <- union(reached, frontier)
  }
  length(reached) == n_records
}

.propagate_alignment <- function(records, reference_index, k) {
  n_records <- length(records)
  aligned <- vector("list", n_records)
  aligned[[reference_index]] <- records[[reference_index]]$sample_labels
  reached <- reference_index
  diagnostics <- list()

  alignment_states <- lapply(
    seq_len(n_records), function(i) .new_alignment_state(n_records, k)
  )
  unreached <- setdiff(seq_len(n_records), reached)
  for (child in unreached) {
    alignment_states[[child]] <- .add_alignment_parent(
      alignment_states[[child]],
      records[[child]]$sample_labels,
      aligned[[reference_index]],
      reference_index,
      k
    )
  }

  while (length(reached) < n_records) {
    candidates <- list()
    cursor <- 0L
    for (child in setdiff(seq_len(n_records), reached)) {
      child_labels <- records[[child]]$sample_labels
      candidate <- .align_labels_from_state(
        child_labels, alignment_states[[child]], k, reached
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
      .abort(paste0(
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
    for (next_child in setdiff(seq_len(n_records), reached)) {
      alignment_states[[next_child]] <- .add_alignment_parent(
        alignment_states[[next_child]],
        records[[next_child]]$sample_labels,
        aligned[[child]],
        child,
        k
      )
    }
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

.complete_coassignment <- function(records, n) {
  coassignment_sum <- matrix(0, nrow = n, ncol = n)
  for (record in records) {
    labels <- record$sample_labels
    cluster_index <- match(labels, unique(labels))
    indicator <- matrix(
      0,
      nrow = n,
      ncol = length(unique(cluster_index)),
      dimnames = list(names(labels), NULL)
    )
    indicator[cbind(seq_len(n), cluster_index)] <- 1
    coassignment_sum <- coassignment_sum + tcrossprod(indicator)
  }
  coassignment_sum / length(records)
}

.consensus_vote <- function(aligned, reference) {
  vapply(seq_len(nrow(aligned)), function(i) {
    observed <- aligned[i, !is.na(aligned[i, ])]
    if (!length(observed)) return(NA_integer_)
    counts <- table(observed)
    candidates <- as.integer(names(counts)[counts == max(counts)])
    if (!is.na(reference[[i]]) && reference[[i]] %in% candidates) {
      reference[[i]]
    } else {
      min(candidates)
    }
  }, integer(1))
}

#' Construct a sample-level consensus partition
#'
#' Partitions with the same `k` are compared through sample co-assignment.
#' Labels are aligned to the consensus partition only after fitting. This label
#' alignment does not align SOM nodes and must not be used to force maps with
#' different grids into a common node geometry.
#'
#' @param partitions A `som_partitions` object.
#' @param k Number of clusters to audit.
#' @param linkage Linkage used to partition the sample co-assignment matrix.
#' @param method Consensus algorithm. `"auto"` uses full co-assignment only
#'   when every partition assigns every sample and the sample count does not
#'   exceed `max_coassignment_n`. It uses medoid-aligned voting for
#'   analysis-scoped resamples with incomplete assignments or larger data.
#' @param max_coassignment_n Largest sample count for an automatically created
#'   dense co-assignment matrix. Set this deliberately because memory grows
#'   quadratically with sample count.
#'
#' @return A `som_consensus` object containing aligned assignments,
#'   partition-assignment, consensus-label and replicated
#'   coverage, membership support, assignment entropy and clusterwise Jaccard
#'   values. The object also records whether the consensus itself retains all
#'   requested clusters and retains the originating partition method as
#'   provenance. For co-assignment consensus, `coassignment` and `tree` contain
#'   the dense matrix and its hierarchical clustering; both are `NULL` for
#'   aligned voting. Aligned voting avoids quadratic growth in sample count, but
#'   alignment and upstream partition auditing can still grow quadratically in
#'   the number of ensemble members.
#' @examples
#' data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 3)
#' specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
#' ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
#' consensus_som(partition_som(ensemble), k = 2)
#' @export
consensus_som <- function(
  partitions,
  k,
  linkage = "average",
  method = c("auto", "coassignment", "aligned_vote"),
  max_coassignment_n = 5000L
) {
  if (!inherits(partitions, "som_partitions")) {
    .abort("`partitions` must come from `partition_som()`.")
  }
  .assert_scalar_integer(k, "k", lower = 2)
  method <- match.arg(method)
  .assert_scalar_integer(
    max_coassignment_n, "max_coassignment_n", lower = 2
  )
  records <- Filter(function(x) x$k == k, partitions$records)
  if (length(records) < 2L) {
    .abort("Consensus auditing requires at least two partitions for the chosen `k`.")
  }
  incomplete_records <- vapply(records, function(record) {
    length(unique(stats::na.omit(record$sample_labels))) != k
  }, logical(1))
  if (any(incomplete_records)) {
    .abort(paste0(
      "Consensus at k = ", k, " requires all k sample-level clusters in ",
      "every partition. Incomplete fits: ",
      paste(
        vapply(records[incomplete_records], `[[`, character(1), "id"),
        collapse = ", "
      ), "."
    ))
  }
  n <- unique(vapply(records, function(x) length(x$sample_labels), integer(1)))
  if (length(n) != 1L) .abort("All partitions must describe the same samples.")
  incomplete <- any(vapply(
    records, function(record) anyNA(record$sample_labels), logical(1)
  ))
  selected_method <- if (method == "auto") {
    if (!incomplete && n <= max_coassignment_n) {
      "coassignment"
    } else {
      "aligned_vote"
    }
  } else {
    method
  }

  coassignment <- tree <- NULL
  alignment_diagnostics <- data.frame(
    parent_fit = character(), n_parent_fits = integer(),
    child_fit = character(), n_joint = integer(),
    n_child_clusters = integer(), n_resolved_clusters = integer(),
    stringsAsFactors = FALSE
  )
  if (selected_method == "coassignment") {
    if (n > max_coassignment_n && method == "coassignment") {
      .abort(paste0(
        "A dense ", n, " x ", n, " co-assignment matrix exceeds ",
        "`max_coassignment_n`; increase the limit explicitly or use ",
        "`method = \"aligned_vote\"`."
      ))
    }
    if (!incomplete) {
      coassignment <- .complete_coassignment(records, n)
    } else {
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
        .abort(
          "Some sample pairs were never jointly assigned across the ensemble."
        )
      }
      coassignment <- coassignment_sum / coassignment_n
    }
    diag(coassignment) <- 1
    tree <- stats::hclust(stats::as.dist(1 - coassignment), method = linkage)
    consensus_labels <- stats::cutree(tree, k = as.integer(k))
  } else {
    if (!.overlap_graph_connected(records)) {
      .abort(paste0(
        "Consensus labels cannot be aligned because the partition-overlap ",
        "graph is disconnected. Use resamples with jointly observed rows or ",
        "interpret the sampling domains separately."
      ))
    }
    reference_index <- .medoid_partition(records)
    reference <- records[[reference_index]]$sample_labels
    propagated <- .propagate_alignment(records, reference_index, k)
    initial_alignment <- propagated$aligned
    alignment_diagnostics <- propagated$diagnostics
    consensus_labels <- .consensus_vote(initial_alignment, reference)
  }

  aligned <- vapply(
    records,
    function(record) .align_labels(record$sample_labels, consensus_labels, k),
    integer(n)
  )
  if (is.null(dim(aligned))) aligned <- matrix(aligned, ncol = 1L)
  colnames(aligned) <- vapply(records, `[[`, character(1), "id")

  assignment_count <- rowSums(!is.na(aligned))
  assignment_coverage <- mean(assignment_count > 0L)
  consensus_label_coverage <- mean(!is.na(consensus_labels))
  observed_consensus_clusters <- sort(unique(stats::na.omit(
    consensus_labels
  )))
  n_consensus_clusters <- length(observed_consensus_clusters)
  complete_consensus_k <- n_consensus_clusters == k
  replicated_assignment_coverage <- mean(assignment_count >= 2L)
  membership_support <- rowMeans(
    aligned == consensus_labels,
    na.rm = TRUE
  )
  membership_support[
    !is.finite(membership_support) | assignment_count < 2L
  ] <- NA_real_
  assignment_entropy <- apply(aligned, 1L, function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) {
      return(NA_real_)
    }
    probability <- tabulate(x, nbins = k) / length(x)
    probability <- probability[probability > 0]
    -sum(probability * log(probability)) / log(k)
  })
  assignment_entropy[assignment_count < 2L] <- NA_real_

  jaccard <- do.call(rbind, lapply(seq_along(records), function(i) {
    do.call(rbind, lapply(seq_len(k), function(cluster) {
      jointly_observed <- assignment_count >= 2L &
        !is.na(consensus_labels) & !is.na(aligned[, i])
      reference_set <- consensus_labels[jointly_observed] == cluster
      candidate_set <- aligned[jointly_observed, i] == cluster
      union <- sum(reference_set | candidate_set)
      data.frame(
        fit_id = records[[i]]$id,
        cluster = cluster,
        n_joint = sum(jointly_observed),
        jaccard = if (union) {
          sum(reference_set & candidate_set) / union
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }))
  }))
  cluster_summary <- do.call(rbind, lapply(split(jaccard, jaccard$cluster), function(x) {
    interval <- .quantile_safe(x$jaccard, c(0.025, 0.975))
    finite <- x$jaccard[is.finite(x$jaccard)]
    data.frame(
      cluster = x$cluster[[1L]],
      median_jaccard = if (length(finite)) stats::median(finite) else NA_real_,
      jaccard_q025 = interval[[1L]],
      jaccard_q975 = interval[[2L]],
      stringsAsFactors = FALSE
    )
  }))

  structure(
    list(
      k = as.integer(k),
      method = selected_method,
      scope = partitions$scope,
      coassignment = coassignment,
      tree = tree,
      consensus_labels = consensus_labels,
      aligned_labels = aligned,
      membership_support = membership_support,
      assignment_entropy = assignment_entropy,
      assignment_count = assignment_count,
      assignment_coverage = assignment_coverage,
      consensus_label_coverage = consensus_label_coverage,
      observed_consensus_clusters = as.integer(observed_consensus_clusters),
      n_consensus_clusters = n_consensus_clusters,
      complete_consensus_k = complete_consensus_k,
      replicated_assignment_coverage = replicated_assignment_coverage,
      alignment_diagnostics = alignment_diagnostics,
      clusterwise_jaccard = jaccard,
      cluster_summary = cluster_summary,
      metadata = partitions$ensemble$data$metadata,
      records = records,
      partition_method = partitions$partition_method %||% partitions$method,
      ensemble = partitions$ensemble
    ),
    class = "som_consensus"
  )
}

#' @export
print.som_consensus <- function(x, ...) {
  cat("<som_consensus>\n")
  cat("  k                  :", x$k, "\n")
  cat("  method             :", x$method, "\n")
  cat("  evidence scope     :", x$scope, "\n")
  cat("  partitions         :", ncol(x$aligned_labels), "\n")
  cat("  samples            :", length(x$consensus_labels), "\n")
  cat("  assigned at least once:", sum(x$assignment_count > 0L), "\n")
  cat("  assignment coverage:", sprintf("%.3f", x$assignment_coverage), "\n")
  cat("  consensus coverage :", sprintf(
    "%.3f", x$consensus_label_coverage %||% x$assignment_coverage
  ), "\n")
  cat("  consensus clusters :", x$n_consensus_clusters %||%
        length(unique(stats::na.omit(x$consensus_labels))), "/", x$k, "\n")
  cat("  replicated coverage:", sprintf(
    "%.3f", x$replicated_assignment_coverage
  ), "\n")
  cat("  median support     :", sprintf(
    "%.3f", stats::median(x$membership_support, na.rm = TRUE)
  ), "\n")
  cat("  median entropy     :", sprintf(
    "%.3f", stats::median(x$assignment_entropy, na.rm = TRUE)
  ), "\n")
  invisible(x)
}
