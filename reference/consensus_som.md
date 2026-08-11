# Construct a sample-level consensus partition

Partitions with the same `k` are compared through sample co-assignment.
Labels are aligned to the consensus partition only after fitting. This
label alignment does not align SOM nodes and must not be used to force
maps with different grids into a common node geometry.

## Usage

``` r
consensus_som(
  partitions,
  k,
  linkage = "average",
  method = c("auto", "coassignment", "aligned_vote"),
  max_coassignment_n = 5000L
)
```

## Arguments

- partitions:

  A `som_partitions` object.

- k:

  Number of clusters to audit.

- linkage:

  Linkage used to partition the sample co-assignment matrix.

- method:

  Consensus algorithm. `"auto"` uses full co-assignment only when every
  partition assigns every sample and the sample count does not exceed
  `max_coassignment_n`. It uses medoid-aligned voting for
  analysis-scoped resamples with incomplete assignments or larger data.

- max_coassignment_n:

  Largest sample count for an automatically created dense co-assignment
  matrix. Set this deliberately because memory grows quadratically with
  sample count.

## Value

A `som_consensus` object containing aligned assignments,
partition-assignment, consensus-label and replicated coverage,
membership support, assignment entropy and clusterwise Jaccard values.
The object also records whether the consensus itself retains all
requested clusters and retains the originating partition method as
provenance. For co-assignment consensus, `coassignment` and `tree`
contain the dense matrix and its hierarchical clustering; both are
`NULL` for aligned voting. Aligned voting avoids quadratic growth in
sample count, but alignment and upstream partition auditing can still
grow quadratically in the number of ensemble members.

## Examples

``` r
data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 3)
specification <- som_spec(c(3, 2), seeds = 1:2, rlen = 10, k = 2)
ensemble <- fit_som_ensemble(data, specification, keep_models = FALSE)
consensus_som(partition_som(ensemble), k = 2)
#> <som_consensus>
#>   k                  : 2 
#>   method             : coassignment 
#>   evidence scope     : analysis 
#>   partitions         : 2 
#>   samples            : 45 
#>   assigned at least once: 45 
#>   assignment coverage: 1.000 
#>   consensus coverage : 1.000 
#>   consensus clusters : 2 / 2 
#>   replicated coverage: 1.000 
#>   median support     : 1.000 
#>   median entropy     : -0.000 
```
