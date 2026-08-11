#' Simulate known-structure data for SOM validation
#'
#' @section Lifecycle:
#' Experimental. Existing scenario names are retained within the 1.x series,
#' while new scenarios and scenario-specific controls may be added in minor
#' releases.
#'
#' @param scenario One of `"gradient"`, `"clusters"`, `"overlap"`,
#'   `"grouped_pseudoreplication"` or `"multilayer_conflict"`.
#' @param n Number of samples.
#' @param p Number of variables per primary layer.
#' @param seed Random seed.
#' @param noise_sd Within-structure residual standard deviation.
#' @param class_probs Three positive class proportions for simulations with
#'   genuine discrete classes. They are normalized internally. Ignored for a
#'   continuous gradient.
#' @param n_groups Number of sampling groups for
#'   `"grouped_pseudoreplication"`. The default is approximately one group per
#'   six observations, with a minimum of 10 groups.
#' @param group_icc Conditional within-class intraclass correlation induced by
#'   the simulated group effect, before any optional domain shift or
#'   missingness. This is not the marginal correlation after variation among
#'   generated class centres is included. Used only for
#'   `"grouped_pseudoreplication"`.
#' @param n_domains Number of balanced sampling domains. A gradient always has
#'   domains spanning its latent coordinate. Other scenarios receive domains
#'   when `domain_shift > 0` or `missing_mechanism = "domain"`.
#' @param domain_shift Magnitude of an additive domain-specific shift applied
#'   to the first two variables. Zero creates no additional additive shift. In
#'   the gradient scenario, domains already occupy successive ranges of the
#'   latent gradient, so a held-domain contrast combines range extrapolation
#'   with any requested additive shift.
#' @param missing_rate Missingness-intensity control. Under `"mcar"`, this is
#'   the entry-level masking probability before minimum-observation safeguards.
#'   Under `"domain"`, entries in the final domain use
#'   `min(0.95, 2 * missing_rate)` and other domains use
#'   `missing_rate / 3`. The realised fraction is recorded in
#'   `simulation_spec$missing_rate_realized`.
#' @param missing_mechanism One of `"none"`, `"mcar"` or `"domain"`. The
#'   `"domain"` mechanism concentrates masking in the final sampling domain;
#'   it is a controlled domain-dependent pattern, not a claim about an
#'   inferential MCAR, MAR or MNAR mechanism.
#' @param id Optional stable sample identifiers. Supply these when independently
#'   constructed simulated objects will be compared in a sensitivity workflow.
#'
#' @return A `som_data` object with additional `truth` and `simulation_spec`
#'   components. Known latent variables and labels are never included in the
#'   training layers. Discrete labels are also copied to `external_label` for
#'   post hoc agreement assessment.
#'
#' @details
#' The scenario catalogue is deliberately compact. `"gradient"` generates a
#' one-dimensional linear latent gradient. `"clusters"` and `"overlap"` use
#' three spherical Gaussian classes with larger and smaller centre separation,
#' respectively. `"grouped_pseudoreplication"` assigns one generated class and
#' one random effect to every sampling group; `class_probs` therefore controls
#' group-class allocation before the approximately balanced group sizes are
#' expanded to rows. `"multilayer_conflict"` combines one class-structured
#' layer with a row-permuted, noise-perturbed copy. These teaching scenarios do
#' not span nonlinear gradients, anisotropic or heavy-tailed clusters, or all
#' ecological dependence and missingness mechanisms.
#' @examples
#' simulated <- simulate_som_scenario(
#'   "gradient", n = 60, p = 4, seed = 1, domain_shift = 0.5
#' )
#' simulated
#' @export
simulate_som_scenario <- function(
  scenario = c(
    "gradient", "clusters", "overlap", "grouped_pseudoreplication",
    "multilayer_conflict"
  ),
  n = 180L, p = 6L, seed = 1L,
  noise_sd = 1,
  class_probs = c(1, 1, 1),
  n_groups = NULL,
  group_icc = 0.7,
  n_domains = 3L,
  domain_shift = 0,
  missing_rate = 0,
  missing_mechanism = c("none", "mcar", "domain"),
  id = NULL
) {
  scenario <- match.arg(scenario)
  missing_mechanism <- match.arg(missing_mechanism)
  .assert_scalar_integer(n, "n", lower = 30)
  .assert_scalar_integer(p, "p", lower = 2)
  .assert_scalar_integer(seed, "seed", lower = 0)
  .assert_scalar_number(noise_sd, "noise_sd", lower = .Machine$double.eps)
  .assert_scalar_number(group_icc, "group_icc", lower = 0, upper = 0.95)
  .assert_scalar_integer(n_domains, "n_domains", lower = 2)
  .assert_scalar_number(domain_shift, "domain_shift", lower = 0)
  .assert_scalar_number(missing_rate, "missing_rate", lower = 0, upper = 0.8)
  n <- as.integer(n)
  p <- as.integer(p)
  n_domains <- as.integer(n_domains)
  if (n_domains > n) .abort("`n_domains` cannot exceed `n`.")
  if (missing_rate > 0 && missing_mechanism == "none") {
    .abort("Choose a missingness mechanism when `missing_rate` is positive.")
  }
  if (missing_rate == 0 && missing_mechanism != "none") {
    .abort("Set `missing_rate` above zero to simulate missingness.")
  }

  discrete <- scenario != "gradient"
  if (discrete) {
    if (!is.numeric(class_probs) || length(class_probs) != 3L ||
          anyNA(class_probs) || any(!is.finite(class_probs)) ||
          any(class_probs <= 0)) {
      .abort("`class_probs` must contain three positive numbers.")
    }
    class_probs <- class_probs / sum(class_probs)
  }
  if (!is.null(n_groups)) {
    .assert_scalar_integer(n_groups, "n_groups", lower = 6, upper = n)
    n_groups <- as.integer(n_groups)
  }

  balanced_ids <- function(size, levels) {
    sample(rep(seq_len(levels), length.out = size), size, replace = FALSE)
  }
  class_ids <- function(size) {
    raw <- size * class_probs
    counts <- floor(raw)
    remainder <- size - sum(counts)
    if (remainder > 0L) {
      add <- order(raw - counts, decreasing = TRUE)[seq_len(remainder)]
      counts[add] <- counts[add] + 1L
    }
    if (any(counts < 2L)) {
      .abort(
        "`n` and `class_probs` must yield at least two observations per class."
      )
    }
    sample(rep(seq_len(3L), counts), size, replace = FALSE)
  }

  generated <- .with_reproducible_seed(as.integer(seed), {
    if (scenario == "gradient") {
      latent <- sort(stats::runif(n, -2, 2))
      basis <- seq(0.6, 1.4, length.out = p)
      x <- outer(latent, basis) +
        matrix(stats::rnorm(n * p, sd = 0.35 * noise_sd), n, p)
      domain_id <- cut(
        rank(latent, ties.method = "first"),
        breaks = seq(0, n, length.out = n_domains + 1L),
        include.lowest = TRUE,
        labels = FALSE
      )
      list(
        layers = list(environment = x), label = NULL, latent = latent,
        group = NULL, domain_id = domain_id
      )
    } else {
      separation <- if (scenario == "overlap") 1.2 else 3.5
      centres <- matrix(0, 3L, p)
      centres[, 1L] <- c(-separation, 0, separation)
      centres[, 2L] <- c(0, separation, 0)

      if (scenario == "grouped_pseudoreplication") {
        group_count <- n_groups %||% max(10L, floor(n / 6L))
        group <- balanced_ids(n, group_count)
        group_label <- class_ids(group_count)
        label <- group_label[group]
        residual <- matrix(stats::rnorm(n * p, sd = noise_sd), n, p)
        group_sd <- noise_sd * sqrt(group_icc / (1 - group_icc))
        group_effect <- matrix(
          stats::rnorm(group_count * p, sd = group_sd),
          nrow = group_count, ncol = p
        )
        x <- centres[label, , drop = FALSE] + residual +
          group_effect[group, , drop = FALSE]
        list(
          layers = list(environment = x), label = label, latent = NULL,
          group = sprintf("site_%03d", group), domain_id = NULL
        )
      } else if (scenario == "multilayer_conflict") {
        label <- class_ids(n)
        x <- centres[label, , drop = FALSE] +
          matrix(stats::rnorm(n * p, sd = noise_sd), n, p)
        conflicting <- x[sample.int(n), , drop = FALSE] +
          matrix(stats::rnorm(n * p, sd = 0.3 * noise_sd), n, p)
        list(
          layers = list(traits = x, environment = conflicting), label = label,
          latent = NULL, group = NULL, domain_id = NULL
        )
      } else {
        label <- class_ids(n)
        x <- centres[label, , drop = FALSE] +
          matrix(stats::rnorm(n * p, sd = noise_sd), n, p)
        list(
          layers = list(environment = x), label = label, latent = NULL,
          group = NULL, domain_id = NULL
        )
      }
    }
  })

  needs_domains <- scenario == "gradient" || domain_shift > 0 ||
    missing_mechanism == "domain"
  if (needs_domains && is.null(generated$domain_id)) {
    generated$domain_id <- .with_reproducible_seed(
      .seed_from_key(seed, "simulation_domains"),
      balanced_ids(n, n_domains)
    )
  }
  if (!is.null(generated$domain_id) && domain_shift > 0) {
    offsets <- seq(-domain_shift, domain_shift, length.out = n_domains)
    shifted_variables <- seq_len(min(2L, p))
    for (nm in names(generated$layers)) {
      generated$layers[[nm]][, shifted_variables] <-
        generated$layers[[nm]][, shifted_variables, drop = FALSE] +
        offsets[generated$domain_id]
    }
  }

  missing_mask <- matrix(FALSE, nrow = n, ncol = p)
  if (missing_rate > 0) {
    missing_mask <- .with_reproducible_seed(
      .seed_from_key(seed, "simulation_missingness"), {
        if (missing_mechanism == "mcar") {
          matrix(stats::runif(n * p) < missing_rate, nrow = n)
        } else {
          probability <- rep(missing_rate / 3, n)
          probability[generated$domain_id == n_domains] <-
            min(0.95, missing_rate * 2)
          matrix(stats::runif(n * p) < probability, nrow = n)
        }
      }
    )
    # Retain at least two measured variables per sample and one value per column.
    for (i in seq_len(n)) {
      if (sum(!missing_mask[i, ]) < 2L) missing_mask[i, seq_len(2L)] <- FALSE
    }
    for (j in seq_len(p)) {
      if (all(missing_mask[, j])) missing_mask[1L, j] <- FALSE
    }
    for (nm in names(generated$layers)) {
      generated$layers[[nm]][missing_mask] <- NA_real_
    }
  }

  for (nm in names(generated$layers)) {
    colnames(generated$layers[[nm]]) <- sprintf("%s_%02d", nm, seq_len(p))
  }
  output <- som_data(
    layers = generated$layers,
    id = id,
    group = generated$group,
    domain = if (is.null(generated$domain_id)) {
      NULL
    } else {
      sprintf("domain_%02d", generated$domain_id)
    },
    external_label = generated$label
  )
  output$truth <- data.frame(
    id = output$metadata$id,
    latent_gradient = generated$latent %||% rep(NA_real_, n),
    class_label = generated$label %||% rep(NA_integer_, n),
    stringsAsFactors = FALSE
  )
  output$simulation_spec <- list(
    scenario = scenario,
    n = n,
    p = p,
    seed = as.integer(seed),
    noise_sd = noise_sd,
    class_probs = if (discrete) class_probs else NULL,
    n_groups = if (scenario == "grouped_pseudoreplication") {
      length(unique(generated$group))
    } else {
      NULL
    },
    group_icc = if (scenario == "grouped_pseudoreplication") {
      group_icc
    } else {
      NULL
    },
    n_domains = if (is.null(generated$domain_id)) 0L else n_domains,
    domain_shift = domain_shift,
    missing_rate_realized = mean(missing_mask),
    missing_mechanism = missing_mechanism
  )
  output
}
