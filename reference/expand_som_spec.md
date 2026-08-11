# Expand an ensemble specification into its model budget

Expand an ensemble specification into its model budget

## Usage

``` r
expand_som_spec(spec)
```

## Arguments

- spec:

  A `som_spec` object.

## Value

A data frame with one row per grid and random seed.

## Examples

``` r
expand_som_spec(som_spec(c(4, 3), seeds = 1:3, rlen = 20, k = 2:3))
#>   xdim ydim grid_id seed   model_id
#> 1    4    3       1    1 g01_4x3_s1
#> 2    4    3       1    2 g01_4x3_s2
#> 3    4    3       1    3 g01_4x3_s3
```
