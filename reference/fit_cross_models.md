# Fit controlled cross-model reference partitions

K-means, Ward.D2 and Gaussian mixture models use the same data object,
resampling indices and leakage-safe preprocessing as a fitted SOM
ensemble. The function creates reference partitions for triangulation;
it does not rank algorithms or define any method as ground truth.

## Usage

``` r
fit_cross_models(
  ensemble,
  methods = c("kmeans", "ward"),
  k = ensemble$spec$k,
  kmeans_seeds = 1L,
  kmeans_nstart = 50L,
  kmeans_iter_max = 100L,
  gmm_model_names = NULL,
  keep_models = FALSE,
  fail_fast = FALSE,
  gmm_seed = 1L
)
```

## Arguments

- ensemble:

  A fitted `som_ensemble` defining data, preprocessing and resampling.

- methods:

  Any of `"kmeans"`, `"ward"` and `"gmm"`.

- k:

  Candidate numbers of clusters.

- kmeans_seeds:

  Random seeds used only for K-means initialization.

- kmeans_nstart:

  Number of random starts per K-means fit.

- kmeans_iter_max:

  Maximum number of iterations per K-means start.

- gmm_model_names:

  Optional covariance models passed to
  [`mclust::Mclust()`](https://mclust-org.github.io/mclust/reference/Mclust.html).
  BIC selection occurs within each analysis split.

- keep_models:

  Whether to retain fitted cross-model objects.

- fail_fast:

  Whether to stop at the first fit failure.

- gmm_seed:

  Base random seed for reproducible GMM initialization. A deterministic
  fit-specific seed is derived from the split and candidate `k`, and is
  retained in the result object.

## Value

A `som_cross_models` object with partitions, warnings and explicit
failures.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 4)
specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
fit_cross_models(ensemble, methods = c("kmeans", "ward"))
#> <som_cross_models>
#>   successful fits: 2 
#>   failed fits    : 0 
#>   warnings       : 0 
#>   methods        : kmeans, ward 
```
