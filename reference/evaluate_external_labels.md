# Compare a consensus partition with external ecological labels

External labels are evaluated only after SOM fitting and consensus
construction. ARI and AMI quantify agreement; neither is called
accuracy, and agreement does not make the external labels ecological
ground truth.

## Usage

``` r
evaluate_external_labels(
  consensus,
  labels = NULL,
  exclude = NULL,
  label_ids = NULL,
  match_by = c("auto", "id", "position")
)
```

## Arguments

- consensus:

  A `som_consensus` object.

- labels:

  Optional external labels. By default, the `external_label` field
  supplied to
  [`som_data()`](https://shaowen-ye.github.io/SOMevidence/reference/som_data.md)
  is used. Supplied labels should be matched by stable sample IDs
  through `label_ids` or complete `names(labels)`.

- exclude:

  Optional label values to omit, such as an explicit unknown category.
  Missing labels and samples without at least two analysis-scope
  assignments are always omitted and counted because one assignment
  cannot provide consensus evidence.

- label_ids:

  Optional unique sample identifiers corresponding to `labels`. They may
  be in any order and may describe a subset of consensus samples. IDs
  not present in the consensus are rejected.

- match_by:

  Matching rule. `"auto"` uses IDs when available. For an unnamed
  full-length label vector it warns and retains legacy positional
  matching. Use `"id"` to require identity matching or `"position"` to
  make full-length positional matching explicit.

## Value

A `som_external_assessment` object containing the contingency table,
long-form cluster composition, ARI, AMI, resolved matching information
and mutually exclusive sample-omission accounting.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 9)
ensemble <- fit_som_ensemble(
  data, som_spec(c(3, 2), seeds = 10:11, rlen = 10, k = 2)
)
consensus <- consensus_som(partition_som(ensemble), k = 2)
labels <- stats::setNames(
  data$metadata$external_label,
  data$metadata$id
)
evaluate_external_labels(consensus, labels[sample(names(labels))])
#> <som_external_assessment> (post hoc)
#>   samples used: 45 of 45 
#>   label match : id 
#>   ARI         : 0.480 
#>   AMI         : 0.616 
#>   interpretation: agreement, not classification accuracy
```
