# Define explicit evidence gates for a hard SOM partition

No universal thresholds are supplied. A gate records the analyst's
prespecified requirements.

## Usage

``` r
som_gate(
  max_topographic_error = NULL,
  max_empty_unit_rate = NULL,
  min_median_ari = NULL,
  min_median_ami = NULL,
  min_pairwise_coverage = NULL,
  min_cluster_jaccard = NULL,
  min_consensus_coverage = NULL,
  min_replicated_coverage = NULL,
  min_membership_support = NULL,
  max_assignment_entropy = NULL,
  min_cross_model_ari = NULL,
  min_cross_model_methods = NULL,
  min_success_rate = NULL
)
```

## Arguments

- max_topographic_error:

  Optional maximum median topographic error.

- max_empty_unit_rate:

  Optional maximum median empty-unit rate.

- min_median_ari:

  Optional minimum median pairwise ARI.

- min_median_ami:

  Optional minimum median pairwise AMI.

- min_pairwise_coverage:

  Optional minimum median proportion of samples jointly assigned in
  pairwise partition comparisons.

- min_cluster_jaccard:

  Optional minimum across cluster-level median Jaccard values.

- min_consensus_coverage:

  Optional minimum proportion of samples with an identifiable consensus
  label.

- min_replicated_coverage:

  Optional minimum proportion of samples assigned by at least two
  partitions, for which support and entropy are estimable.

- min_membership_support:

  Optional minimum median sample membership support.

- max_assignment_entropy:

  Optional maximum median normalized assignment entropy.

- min_cross_model_ari:

  Optional minimum of the method-specific median SOM-to-reference ARI
  values. Each requested reference method must have an evaluable result;
  pooled comparisons are not used for this gate.

- min_cross_model_methods:

  Optional minimum number of reference methods represented in the
  comparison.

- min_success_rate:

  Optional minimum model-fit success rate.

## Value

A `som_gate` object.

## Examples

``` r
som_gate(min_success_rate = 0.95, min_cluster_jaccard = 0.75)
#> <som_gate> (analyst-specified)
#>    min_cluster_jaccard : 0.75 
#>    min_success_rate : 0.95 
```
