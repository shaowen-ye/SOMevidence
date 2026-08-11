# Upgrade a persisted SOMevidence object contract

Upgrades a supported public SOMevidence object to the current structural
contract using only deterministic, non-analytical migrations. The
function never refits a model or fabricates scientific evidence. If a
required field cannot be reconstructed, it asks the user to recompute
the affected result.

## Usage

``` r
upgrade_som_object(x)
```

## Arguments

- x:

  A persisted public `som_*` object returned by SOMevidence.

## Value

A validated object under the current contract. The input is not modified
in place. Repeated upgrades are idempotent.

## Details

Contract versions describe object structure, not the package or
algorithm version that generated an analysis. Retain
`som_workflow$provenance` and
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) for that
purpose.

## Examples

``` r
data <- som_data(matrix(seq_len(18), nrow = 6))
legacy <- data
attr(legacy, "som_contract_version") <- NULL
upgrade_som_object(legacy)
#> <som_data>
#>   samples: 6 
#>   layers : 1 
#>   id source: generated 
#>     - data: 3 variables, 0.0% missing
#>   design : sample id only 
```
