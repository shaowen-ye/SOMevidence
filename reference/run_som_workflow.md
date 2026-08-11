# Run a complete SOM defensibility workflow

The workflow preserves representation quality, partition stability,
sample-level consensus and cross-model agreement as separate evidence
streams. It does not combine them into a score or select a winning `k`.

## Usage

``` r
run_som_workflow(
  data,
  spec,
  resamples = NULL,
  preprocess = som_preprocess(),
  k = spec$k,
  cross_models = c("kmeans", "ward"),
  cross_model_control = list(),
  consensus_method = "auto",
  max_coassignment_n = 5000L,
  keep_models = FALSE,
  fail_fast = FALSE,
  parallel = FALSE,
  max_pairwise_comparisons = 1000000L
)
```

## Arguments

- data:

  A `som_data` object.

- spec:

  A `som_spec` object.

- resamples:

  Optional `som_resamples` object. If `NULL`, a single full-data split
  is used; that run does not assess perturbation under resampling.

- preprocess:

  One `som_preprocess` object or a named object per layer.

- k:

  Candidate numbers of hard partitions.

- cross_models:

  Controlled reference methods passed to
  [`fit_cross_models()`](https://shaowen-ye.github.io/SOMevidence/reference/fit_cross_models.md).
  Use [`character()`](https://rdrr.io/r/base/character.html) to skip
  cross-model fitting.

- cross_model_control:

  Named list of optional `kmeans_seeds`, `kmeans_nstart`,
  `kmeans_iter_max`, `gmm_model_names` or `gmm_seed` settings forwarded
  to
  [`fit_cross_models()`](https://shaowen-ye.github.io/SOMevidence/reference/fit_cross_models.md).
  Methods, candidate `k`, model retention and failure policy remain
  controlled by this workflow.

- consensus_method:

  Consensus method passed to
  [`consensus_som()`](https://shaowen-ye.github.io/SOMevidence/reference/consensus_som.md).

- max_coassignment_n:

  Dense co-assignment limit passed to
  [`consensus_som()`](https://shaowen-ye.github.io/SOMevidence/reference/consensus_som.md).

- keep_models:

  Whether to retain fitted SOM and reference model objects.

- fail_fast:

  Whether a model or consensus failure should stop the run.

- parallel:

  Whether SOM ensemble members should use the current `future` plan;
  passed to
  [`fit_som_ensemble()`](https://shaowen-ye.github.io/SOMevidence/reference/fit_som_ensemble.md).

- max_pairwise_comparisons:

  Pairwise partition-comparison budget passed to
  [`partition_som()`](https://shaowen-ye.github.io/SOMevidence/reference/partition_som.md).

## Value

A `som_workflow` object containing each evidence stream and explicit
failure logs.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 6)
specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2:3)
workflow <- run_som_workflow(
  data, specification, cross_models = c("kmeans", "ward")
)
summary(workflow)
#> <summary.som_workflow>
#>   SOM success: 2 / 2 
#>   consensus  : 2 computed; 0 not_computed
#>     - k=2: computed; complete=yes; assignment=100.0%; labels=100.0%; replicated=100.0%
#>     - k=3: computed; complete=yes; assignment=100.0%; labels=100.0%; replicated=100.0%
#>   reference methods:
#>     - kmeans: 2 expected, 2 succeeded, 0 failed, 0 warnings, 100.0% success
#>     - ward: 2 expected, 2 succeeded, 0 failed, 0 warnings, 100.0% success
```
