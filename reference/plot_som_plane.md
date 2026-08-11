# Plot an interpretable plane from one SOM ensemble member

This function deliberately plots a named ensemble member instead of
silently treating one fitted map as representative of the ensemble.
Component values are on the processed scale used for training.

## Usage

``` r
plot_som_plane(
  x,
  type = c("component", "occupancy", "neighbour_distance"),
  fit_id = NULL,
  layer = NULL,
  variables = NULL
)
```

## Arguments

- x:

  A `som_ensemble` object.

- type:

  One of `"component"`, `"occupancy"` or `"neighbour_distance"`.

- fit_id:

  Identifier of one successful ensemble fit. It may be omitted only when
  the ensemble contains exactly one successful fit.

- layer:

  Layer name used for a component plane. It may be omitted for a
  single-layer analysis.

- variables:

  Optional component names. All variables in `layer` are shown by
  default.

## Value

An editable `ggplot` object.
