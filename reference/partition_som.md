# Form and compare candidate hard partitions of SOM units

Form and compare candidate hard partitions of SOM units

## Usage

``` r
partition_som(
  ensemble,
  k = ensemble$spec$k,
  method = "ward.D2",
  scope = c("analysis", "all"),
  max_pairwise_comparisons = 1000000L
)
```

## Arguments

- ensemble:

  A fitted `som_ensemble`.

- k:

  Candidate numbers of clusters. Defaults to the ensemble specification.

- method:

  Hierarchical clustering method used on SOM codebooks.

- scope:

  Evidence scope. The default, `"analysis"`, masks every row not used to
  fit a given ensemble member. `"all"` explicitly audits the stability
  of mapped labels for all rows and must not be used as training-
  partition evidence in
  [`assess_defensibility()`](https://shaowen-ye.github.io/SOMevidence/reference/assess_defensibility.md).

- max_pairwise_comparisons:

  Maximum number of pairwise partition comparisons created across all
  requested `k`. This explicit budget guards against quadratic growth in
  the number of ensemble members.

## Value

A `som_partitions` object. Agreement is reported as ARI and AMI, not
accuracy. AMI is chance-adjusted and normalized by the arithmetic mean
of the two partition entropies.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 2)
specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
partition_som(ensemble)
#> <som_partitions>
#>   fitted partitions : 2 
#>   candidate k       : 2 
#>   evidence scope    : analysis 
#>   pairwise contrasts: 1 
#>   agreement metric  : adjusted Rand index (not accuracy)
```
