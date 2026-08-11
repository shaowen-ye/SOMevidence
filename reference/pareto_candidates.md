# Identify Pareto-efficient SOM candidates

When `x` is a `som_audit`, fit-level metrics are directly comparable
only when every successful fit uses the same analysis rows. To preserve
the established interface, an audit with distinct analysis sets still
returns its exploratory fit-level frontier, but emits a warning and
marks the result with
`attr(result, "comparison_scope") == "distinct_analysis_sets"`. Such a
frontier must not be used to choose a configuration. Instead, construct
prespecified configuration-level summaries over identical successful
split and seed coverage, then pass that justified comparable data frame.

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

  A `som_audit` object or a data frame of comparable candidates.

- metrics:

  Named character vector whose values are `"min"` or `"max"`.

## Value

The non-dominated rows, with a `pareto` column. Results derived from a
`som_audit` also carry a `comparison_scope` attribute.
