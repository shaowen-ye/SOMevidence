# Simulate known-structure data for SOM validation

Simulate known-structure data for SOM validation

## Usage

``` r
simulate_som_scenario(
  scenario = c("gradient", "clusters", "overlap", "grouped_pseudoreplication",
    "multilayer_conflict"),
  n = 180L,
  p = 6L,
  seed = 1L,
  noise_sd = 1,
  class_probs = c(1, 1, 1),
  n_groups = NULL,
  group_icc = 0.7,
  n_domains = 3L,
  domain_shift = 0,
  missing_rate = 0,
  missing_mechanism = c("none", "mcar", "domain"),
  id = NULL
)
```

## Arguments

- scenario:

  One of `"gradient"`, `"clusters"`, `"overlap"`,
  `"grouped_pseudoreplication"` or `"multilayer_conflict"`.

- n:

  Number of samples.

- p:

  Number of variables per primary layer.

- seed:

  Random seed.

- noise_sd:

  Within-structure residual standard deviation.

- class_probs:

  Three positive class proportions for simulations with genuine discrete
  classes. They are normalized internally. Ignored for a continuous
  gradient.

- n_groups:

  Number of sampling groups for `"grouped_pseudoreplication"`. The
  default is approximately one group per six observations, with a
  minimum of 10 groups.

- group_icc:

  Conditional within-class intraclass correlation induced by the
  simulated group effect, before any optional domain shift or
  missingness. This is not the marginal correlation after variation
  among generated class centres is included. Used only for
  `"grouped_pseudoreplication"`.

- n_domains:

  Number of balanced sampling domains. A gradient always has domains
  spanning its latent coordinate. Other scenarios receive domains when
  `domain_shift > 0` or `missing_mechanism = "domain"`.

- domain_shift:

  Magnitude of an additive domain-specific shift applied to the first
  two variables. Zero creates no additional additive shift. In the
  gradient scenario, domains already occupy successive ranges of the
  latent gradient, so a held-domain contrast combines range
  extrapolation with any requested additive shift.

- missing_rate:

  Missingness-intensity control. Under `"mcar"`, this is the entry-level
  masking probability before minimum-observation safeguards. Under
  `"domain"`, entries in the final domain use
  `min(0.95, 2 * missing_rate)` and other domains use
  `missing_rate / 3`. The realised fraction is recorded in
  `simulation_spec$missing_rate_realized`.

- missing_mechanism:

  One of `"none"`, `"mcar"` or `"domain"`. The `"domain"` mechanism
  concentrates masking in the final sampling domain; it is a controlled
  domain-dependent pattern, not a claim about an inferential MCAR, MAR
  or MNAR mechanism.

- id:

  Optional stable sample identifiers. Supply these when independently
  constructed simulated objects will be compared in a sensitivity
  workflow.

## Value

A `som_data` object with additional `truth` and `simulation_spec`
components. Known latent variables and labels are never included in the
training layers. Discrete labels are also copied to `external_label` for
post hoc agreement assessment.

## Details

The scenario catalogue is deliberately compact. `"gradient"` generates a
one-dimensional linear latent gradient. `"clusters"` and `"overlap"` use
three spherical Gaussian classes with larger and smaller centre
separation, respectively. `"grouped_pseudoreplication"` assigns one
generated class and one random effect to every sampling group;
`class_probs` therefore controls group-class allocation before the
approximately balanced group sizes are expanded to rows.
`"multilayer_conflict"` combines one class-structured layer with a
row-permuted, noise-perturbed copy. These teaching scenarios do not span
nonlinear gradients, anisotropic or heavy-tailed clusters, or all
ecological dependence and missingness mechanisms.

## Lifecycle

Experimental. Existing scenario names are retained within the 1.x
series, while new scenarios and scenario-specific controls may be added
in minor releases.

## Examples

``` r
simulated <- simulate_som_scenario(
  "gradient", n = 60, p = 4, seed = 1, domain_shift = 0.5
)
simulated
#> <som_data>
#>   samples: 60 
#>   layers : 1 
#>   id source: generated 
#>     - environment: 4 variables, 0.0% missing
#>   design : domain 
```
