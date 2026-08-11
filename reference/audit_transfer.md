# Audit transfer to held-out sampling domains

For each successful fit with assessment rows, the function contrasts
mapping error in the analysis and assessment sets and records the
fraction of held- out samples mapped to units unoccupied during
training. These quantities describe domain shift; they do not establish
ecological transferability on their own.

## Usage

``` r
audit_transfer(ensemble)
```

## Arguments

- ensemble:

  A fitted `som_ensemble` containing non-empty assessment sets.

## Value

A `som_transfer_audit` object with fit-level transfer diagnostics.

## Examples

``` r
data <- simulate_som_scenario("gradient", n = 60, p = 3, seed = 7)
splits <- som_resamples(data, method = "leave_domain_out")
specification <- som_spec(c(3, 2), seeds = 1, rlen = 10, k = 2)
ensemble <- fit_som_ensemble(data, specification, splits)
audit_transfer(ensemble)
#> <som_transfer_audit>
#>   successful transfers: 3 
#>   median distance ratio: 13.274 
#>   median held-out coverage: 1.000 
#>   median new-unit rate : 0.000 
```
