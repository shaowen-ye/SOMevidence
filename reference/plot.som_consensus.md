# Plot sample-level consensus evidence

Plot sample-level consensus evidence

## Usage

``` r
# S3 method for class 'som_consensus'
plot(
  x,
  type = c("support", "heatmap"),
  samples = NULL,
  max_samples = 500L,
  ...
)
```

## Arguments

- x:

  A `som_consensus` object.

- type:

  Either `"support"` or `"heatmap"`. A heatmap requires a co-assignment
  consensus; aligned voting does not create a dense co-assignment
  matrix.

- samples:

  Optional sample indices for a heatmap.

- max_samples:

  Maximum heatmap size allowed without explicit `samples`.

- ...:

  Unused.

## Value

A `ggplot` object.
