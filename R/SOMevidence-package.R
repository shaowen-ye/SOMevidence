#' SOMevidence: evidence-oriented auditing of self-organizing maps
#'
#' `SOMevidence` wraps the training implementation in [kohonen::supersom()]
#' with explicit data-design,
#' preprocessing, resampling, ensemble and evidence-auditing interfaces. Its
#' central distinction is between continuous-representation diagnostics and a
#' defensible hard partition.
#'
#' @section API contract:
#' The package ships machine-readable contracts for exported functions and
#' returned objects in `inst/extdata`. The Shiny interface, sensitivity
#' scenario list and simulation catalogue are marked `experimental`; all other
#' exported interfaces form the stable 1.0.0 API.
#'
#' The public API is deliberately evidence-oriented. It keeps representation,
#' partition, cross-model, external-label and transfer evidence separate and
#' permits an explicit insufficient-evidence outcome. The package does not
#' claim to validate ecological truth or choose a universally optimal number
#' of clusters.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "quantization_error", "topographic_error", "empty_unit_rate", "grid",
  "support", "entropy", "cluster", "column", "row", "coassignment",
  "k", "agreement", "method", "metric", "grid_id",
  "median_analysis_distance", "median_assessment_distance",
  "unoccupied_unit_rate"
))
