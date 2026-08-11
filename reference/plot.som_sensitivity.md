# Plot sample-level sensitivity across prespecified scenarios

Each point represents one sample at one candidate `k`. The horizontal
axis conditions cluster-member sets on jointly evaluable shared samples
and therefore isolates repartitioning. The vertical axis also includes
eligible members unique to either scenario and therefore combines data
coverage with repartitioning. Values are medians across evaluable
contrasts. Membership sets are compared directly, so the plot is
invariant to arbitrary cluster-label permutations. It describes
sensitivity to the scenarios that were supplied, not a probability that
a cluster assignment is correct.

## Usage

``` r
# S3 method for class 'som_sensitivity'
plot(x, k = NULL, ...)
```

## Arguments

- x:

  A `som_sensitivity` object.

- k:

  Optional candidate cluster counts to display.

- ...:

  Unused.

## Value

An editable `ggplot` object.
