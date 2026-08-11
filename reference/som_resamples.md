# Construct design-aware resamples

Construct design-aware resamples

## Usage

``` r
som_resamples(
  data,
  method = c("full", "subsample", "group_subsample", "block_subsample",
    "leave_domain_out", "custom"),
  repeats = 50L,
  prop = 0.8,
  seed = 1L,
  unit = "group",
  domain = "domain",
  splits = NULL
)
```

## Arguments

- data:

  A `som_data` object.

- method:

  One of `"full"`, `"subsample"`, `"group_subsample"`,
  `"block_subsample"`, `"leave_domain_out"` or `"custom"`.

- repeats:

  Number of repeated subsamples.

- prop:

  Proportion of rows or sampling units retained for analysis.

- seed:

  Random seed used to generate all splits.

- unit:

  Metadata column name or vector defining groups or predefined blocks.
  `block_subsample` does not infer blocks from row order.

- domain:

  Metadata column name or vector defining transfer domains.

- splits:

  For `method = "custom"`, a list containing `analysis` and optional
  `assessment` row indices.

## Value

A `som_resamples` object. The object records the number of distinct
analysis sets in `n_unique_analysis_splits` and identifies repeated sets
in `duplicate_analysis_splits`.

## Examples

``` r
x <- matrix(seq_len(120), nrow = 30)
data <- som_data(x, group = rep(paste0("site_", 1:10), each = 3))
som_resamples(data, method = "group_subsample", repeats = 2, seed = 1)
#> <som_resamples>
#>   method    : group_subsample 
#>   splits    : 2 
#>   unique analysis sets: 2 
#>   analysis  : 24-24 rows
#>   assessment: 6-6 rows
```
