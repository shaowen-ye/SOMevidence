# Map new observations through a fitted SOM ensemble

Every ensemble member applies only the transformations, centering,
scaling and layer geometry learned from its own analysis split. The
function reports best-matching units and mapping distances separately by
fit; it does not align nodes across different grids or call the result
prediction accuracy.

## Usage

``` r
map_som_ensemble(ensemble, new_data, fail_fast = FALSE)
```

## Arguments

- ensemble:

  A `som_ensemble` fitted with `keep_models = TRUE`.

- new_data:

  A `som_data` object with the same named layers and variables as the
  training data. Metadata may differ and is retained.

- fail_fast:

  Whether to stop at the first member-specific mapping error.

## Value

A `som_newdata_mapping` object with sample-by-fit records, fit-level
distance and occupancy summaries, failures and new-data metadata.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 8)
specification <- som_spec(c(3, 2), seeds = 1, rlen = 10, k = 2)
ensemble <- fit_som_ensemble(data, specification, keep_models = TRUE)
new_data <- som_data(layers = list(
  environment = data$layers$environment[1:3, , drop = FALSE]
))
map_som_ensemble(ensemble, new_data)
#> <som_newdata_mapping>
#>   new samples    : 3 
#>   fits attempted : 1 
#>   fits mapped    : 1 
#>   fits failed    : 0 
#>   warnings       : 0 
#>   node consensus : not computed across grids
```
