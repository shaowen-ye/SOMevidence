# Specify a reproducible SOM ensemble

Specify a reproducible SOM ensemble

## Usage

``` r
som_spec(
  grids = data.frame(xdim = 7L, ydim = 5L),
  seeds = 1:10,
  rlen = 500L,
  alpha = c(0.05, 0.01),
  radius = NULL,
  topology = c("hexagonal", "rectangular"),
  neighbourhood = c("gaussian", "bubble"),
  toroidal = FALSE,
  mode = c("batch", "online", "pbatch"),
  k = 2:8,
  max_na_fraction = 0,
  layer_weights = NULL,
  normalize_layers = TRUE,
  cores = 1L
)
```

## Arguments

- grids:

  A two-column data frame with `xdim` and `ydim`, a numeric vector of
  length two for one grid, or a list of such vectors.

- seeds:

  Integer random seeds.

- rlen:

  Number of training iterations.

- alpha:

  Initial and final learning rates.

- radius:

  Optional initial and final neighbourhood radii.

- topology:

  Rectangular or hexagonal grid.

- neighbourhood:

  Bubble or Gaussian neighbourhood.

- toroidal:

  Whether the grid is toroidal.

- mode:

  Training mode supported by
  [`kohonen::supersom()`](https://rdrr.io/pkg/kohonen/man/supersom.html).

- k:

  Candidate numbers of hard partitions retained for later auditing.

- max_na_fraction:

  Maximum allowed fraction of missing variables per layer and sample
  passed to `kohonen`.

- layer_weights:

  Optional non-negative scientific weights, one per layer.

- normalize_layers:

  Whether to divide each scientific layer weight by its training-set
  mean squared Euclidean distance. The package uses the variance
  identity for a deterministic, leakage-safe estimate and applies the
  same effective weights to SOM and cross-model fits.

- cores:

  Number of cores used by `kohonen` in parallel batch mode.

## Value

A `som_spec` object.

## Examples

``` r
specification <- som_spec(
  grids = list(c(4, 3), c(5, 4)), seeds = 1:2, rlen = 50, k = 2:4
)
expand_som_spec(specification)
#>   xdim ydim grid_id seed   model_id
#> 1    4    3       1    1 g01_4x3_s1
#> 2    4    3       1    2 g01_4x3_s2
#> 3    5    4       2    1 g02_5x4_s1
#> 4    5    4       2    2 g02_5x4_s2
```
