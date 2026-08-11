# Compare a consensus partition with external ecological labels

External labels are evaluated only after SOM fitting and consensus
construction. ARI and AMI quantify agreement; neither is called
accuracy, and agreement does not make the external labels ecological
ground truth.

## Usage

``` r
evaluate_external_labels(consensus, labels = NULL, exclude = NULL)
```

## Arguments

- consensus:

  A `som_consensus` object.

- labels:

  Optional external labels in sample order. By default, the
  `external_label` field supplied to
  [`som_data()`](https://shaowen-ye.github.io/SOMevidence/reference/som_data.md)
  is used.

- exclude:

  Optional label values to omit, such as an explicit unknown category.
  Missing labels and samples without at least two analysis-scope
  assignments are always omitted and counted because one assignment
  cannot provide consensus evidence.

## Value

A `som_external_assessment` object containing the contingency table,
long-form cluster composition, ARI, AMI and sample accounting.
