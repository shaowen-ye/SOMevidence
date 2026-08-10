.align_labels <- function(labels, reference, k) {
  keep <- !is.na(labels) & !is.na(reference)
  contingency <- table(
    factor(labels[keep], levels = seq_len(k)),
    factor(reference[keep], levels = seq_len(k))
  )
  assignment <- clue::solve_LSAP(contingency, maximum = TRUE)
  mapping <- as.integer(assignment)
  mapping[rowSums(contingency) == 0L] <- NA_integer_
  aligned <- rep(NA_integer_, length(labels))
  aligned[!is.na(labels)] <- mapping[as.integer(labels[!is.na(labels)])]
  aligned
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
#' @return A `som_consensus` object containing the co-assignment matrix,
#'   aligned assignments, assignment and replicated-assignment coverage,
#'   membership support, assignment entropy and clusterwise Jaccard values.
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
  .assert_scalar_number(k, "k", lower = 2)
  method <- match.arg(method)
  .assert_scalar_number(max_coassignment_n, "max_coassignment_n", lower = 2)
  records <- Filter(function(x) x$k == k, partitions$records)
  if (length(records) < 2L) {
    .abort("Consensus auditing requires at least two partitions for the chosen `k`.")
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
  if (selected_method == "coassignment") {
    if (n > max_coassignment_n && method == "coassignment") {
      .abort(paste0(
        "A dense ", n, " x ", n, " co-assignment matrix exceeds ",
        "`max_coassignment_n`; increase the limit explicitly or use ",
        "`method = \"aligned_vote\"`."
      ))
    }
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
      .abort("Some sample pairs were never jointly assigned across the ensemble.")
    }
    coassignment <- coassignment_sum / coassignment_n
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
    initial_alignment <- vapply(
      records,
      function(record) .align_labels(record$sample_labels, reference, k),
      integer(n)
    )
    if (is.null(dim(initial_alignment))) {
      initial_alignment <- matrix(initial_alignment, ncol = 1L)
    }
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
    data.frame(
      cluster = x$cluster[[1L]],
      median_jaccard = stats::median(x$jaccard, na.rm = TRUE),
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
      replicated_assignment_coverage = replicated_assignment_coverage,
      clusterwise_jaccard = jaccard,
      cluster_summary = cluster_summary,
      metadata = partitions$ensemble$data$metadata,
      records = records,
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
