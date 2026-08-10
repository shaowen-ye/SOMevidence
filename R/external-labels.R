#' Compare a consensus partition with external ecological labels
#'
#' External labels are evaluated only after SOM fitting and consensus
#' construction. ARI and AMI quantify agreement; neither is called accuracy,
#' and agreement does not make the external labels ecological ground truth.
#'
#' @param consensus A `som_consensus` object.
#' @param labels Optional external labels in sample order. By default, the
#'   `external_label` field supplied to [som_data()] is used.
#' @param exclude Optional label values to omit, such as an explicit unknown
#'   category. Missing labels and samples without at least two analysis-scope
#'   assignments are always omitted and counted because one assignment cannot
#'   provide consensus evidence.
#'
#' @return A `som_external_assessment` object containing the contingency table,
#'   long-form cluster composition, ARI, AMI and sample accounting.
#' @export
evaluate_external_labels <- function(consensus, labels = NULL, exclude = NULL) {
  if (!inherits(consensus, "som_consensus")) {
    .abort("`consensus` must come from `consensus_som()`.")
  }
  source <- "supplied"
  if (is.null(labels)) {
    if (!"external_label" %in% names(consensus$metadata)) {
      .abort("No external labels are stored; supply `labels` explicitly.")
    }
    labels <- consensus$metadata$external_label
    source <- "som_data.external_label"
  }
  if (length(labels) != length(consensus$consensus_labels)) {
    .abort("`labels` must contain one value per consensus sample.")
  }

  keep <- !is.na(labels) & !is.na(consensus$consensus_labels) &
    consensus$assignment_count >= 2L
  if (!is.null(exclude)) keep <- keep & !labels %in% exclude
  if (sum(keep) < 2L || length(unique(labels[keep])) < 2L) {
    .abort("External assessment requires at least two represented label levels.")
  }
  partition <- consensus$consensus_labels[keep]
  external <- labels[keep]
  contingency <- table(
    consensus_cluster = partition,
    external_label = external,
    useNA = "no"
  )
  composition <- as.data.frame(contingency, stringsAsFactors = FALSE)
  names(composition)[[3L]] <- "n"
  cluster_n <- stats::ave(
    composition$n,
    composition$consensus_cluster,
    FUN = sum
  )
  composition$within_cluster_proportion <- composition$n / cluster_n

  structure(
    list(
      n_total = length(labels),
      n_used = sum(keep),
      n_omitted = sum(!keep),
      source = source,
      excluded_values = exclude,
      ari = .adjusted_rand(partition, external),
      ami = .adjusted_mutual_info(partition, external),
      contingency = contingency,
      composition = composition
    ),
    class = "som_external_assessment"
  )
}

#' @export
print.som_external_assessment <- function(x, ...) {
  cat("<som_external_assessment> (post hoc)\n")
  cat("  samples used:", x$n_used, "of", x$n_total, "\n")
  cat("  ARI         :", sprintf("%.3f", x$ari), "\n")
  cat("  AMI         :", sprintf("%.3f", x$ami), "\n")
  cat("  interpretation: agreement, not classification accuracy\n")
  invisible(x)
}
