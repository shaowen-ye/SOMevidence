# Construct a design-explicit SOM data object

Construct a design-explicit SOM data object

## Usage

``` r
som_data(
  x = NULL,
  layers = NULL,
  id = NULL,
  group = NULL,
  time = NULL,
  domain = NULL,
  weight = NULL,
  external_label = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame for a single-layer analysis.

- layers:

  A named list of numeric matrices or data frames. Use either `x` or
  `layers`, not both.

- id:

  Optional unique sample identifiers.

- group:

  Optional sampling-group identifiers.

- time:

  Optional time values retained as metadata.

- domain:

  Optional monitoring-domain identifiers.

- weight:

  Optional non-negative survey or summary weights. These weights are
  retained only as metadata in the current release. They are not used in
  SOM training, evidence metrics or design summaries.

- external_label:

  Optional external ecological labels. They are retained for post hoc
  assessment and never added to the training layers.

## Value

An object of class `som_data`. Named multi-layer inputs are aligned to
the first layer by row name when all layers provide row names;
incompatible or partly named layers are rejected. An explicit `id`
vector is always interpreted positionally against the first layer and
never changes its row order. If no layers have usable row names, their
existing row order is treated as authoritative. The scalar `id_source`
records whether sample identifiers were supplied, taken from explicit
non-positional row names or generated locally. Stable identifiers are
required to compare different data-coverage scenarios.

## Examples

``` r
x <- data.frame(temperature = 10:14, oxygen = c(9, 8, 8, 7, 6))
data <- som_data(x, id = paste0("sample_", seq_len(nrow(x))))
data
#> <som_data>
#>   samples: 5 
#>   layers : 1 
#>   id source: provided 
#>     - data: 2 variables, 0.0% missing
#>   design : sample id only 
```
