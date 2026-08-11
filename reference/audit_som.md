# Audit representation quality across a SOM ensemble

Audit representation quality across a SOM ensemble

## Usage

``` r
audit_som(ensemble)
```

## Arguments

- ensemble:

  A fitted `som_ensemble`.

## Value

A `som_audit` object containing fit-level metrics and summaries.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 1)
specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
audit_som(fit_som_ensemble(data, specification, keep_models = FALSE))
#> <som_audit>
#>   successful fits: 2 
#>   success rate   : 100.0% 
#>   grids audited  : 1 
```
