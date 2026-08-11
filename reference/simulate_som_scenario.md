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

  Target intraclass correlation induced by the simulated group effect.
  Used only for `"grouped_pseudoreplication"`.

- n_domains:

  Number of balanced sampling domains. A gradient always has domains
  spanning its latent coordinate. Other scenarios receive domains when
  `domain_shift > 0` or `missing_mechanism = "domain"`.

- domain_shift:

  Magnitude of an additive domain-specific shift applied to the first
  two variables. Zero creates no additional domain shift.

- missing_rate:

  Expected fraction of entries set to missing.

- missing_mechanism:

  One of `"none"`, `"mcar"` or `"domain"`. Domain missingness is
  concentrated in the final sampling domain.

- id:

  Optional stable sample identifiers. Supply these when independently
  constructed simulated objects will be compared in a sensitivity
  workflow.

## Value

A `som_data` object with additional `truth` and `simulation_spec`
components. Known latent variables and labels are never included in the
training layers. Discrete labels are also copied to `external_label` for
post hoc agreement assessment.

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
