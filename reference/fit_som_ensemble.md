# Fit a reproducible ensemble of self-organizing maps

Every model receives preprocessing parameters estimated only from its
own analysis rows. Failures are recorded instead of silently discarded.
Topographic error is computed during fitting so duplicate full processed
matrices need not be retained in every ensemble member.

## Usage

``` r
fit_som_ensemble(
  data,
  spec,
  resamples = NULL,
  preprocess = som_preprocess(),
  keep_models = TRUE,
  fail_fast = FALSE,
  parallel = FALSE
)
```

## Arguments

- data:

  A `som_data` object.

- spec:

  A `som_spec` object.

- resamples:

  A `som_resamples` object. The default fits the full data.

- preprocess:

  One `som_preprocess` object or a named object per layer.

- keep_models:

  Whether to retain fitted `kohonen` objects.

- fail_fast:

  Whether to stop at the first model failure.

- parallel:

  Whether to distribute ensemble members through the current `future`
  plan using `future.apply`. The package never changes that plan.
  Explicit model seeds retain reproducibility across sequential and
  future execution. Avoid combining worker-level parallelism with
  `spec$cores > 1` unless nested parallelism is intended.

## Value

A `som_ensemble` object.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 1)
specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
ensemble
#> <som_ensemble>
#>   attempted: 2 
#>   succeeded: 2 
#>   failed   : 0 
#>   warnings : 0 
#>   samples  : 45 
```
