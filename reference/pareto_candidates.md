# Identify Pareto-efficient SOM candidates

Identify Pareto-efficient SOM candidates

## Usage

``` r
pareto_candidates(
  x,
  metrics = c(quantization_error = "min", topographic_error = "min", empty_unit_rate =
    "min")
)
```

## Arguments

- x:

  A `som_audit` object or data frame.

- metrics:

  Named character vector whose values are `"min"` or `"max"`.

## Value

The non-dominated rows, with a `pareto` column.
