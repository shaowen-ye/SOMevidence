# Compare SOM partitions with controlled cross-model references

Compare SOM partitions with controlled cross-model references

## Usage

``` r
compare_cross_models(
  partitions,
  cross_models,
  scope = c("analysis", "assessment", "all")
)
```

## Arguments

- partitions:

  A `som_partitions` object.

- cross_models:

  A `som_cross_models` object.

- scope:

  Compare analysis rows (`"analysis"`), assessment rows when available
  (`"assessment"`) or all mapped rows (`"all"`). The analysis scope is
  the default for hard-partition defensibility; assessment scope is a
  separate mapping-transfer diagnostic.

## Value

A `som_cross_comparison` containing ARI and AMI effect sizes, summaries,
reference-fit success rates, source-partition completeness and
source-partition provenance. Neither agreement metric is reported as
accuracy.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 5)
specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
partitions <- partition_som(ensemble)
references <- fit_cross_models(ensemble, methods = "ward")
compare_cross_models(partitions, references)
#> <som_cross_comparison>
#>   comparisons: 2 
#>   scope      : analysis 
#>   metrics    : ARI and AMI (agreement, not accuracy)
```
