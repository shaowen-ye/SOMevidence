# Run prespecified SOM sensitivity scenarios

Each named scenario must provide `data` and `spec`, and may provide
`resamples`, `preprocess` and `k`. Scenarios may represent alternative
variable modules, transformations, layer weights, grids or data-coverage
rules. Results remain separate by scenario and evidence stream; the
function creates no aggregate ranking.

## Usage

``` r
run_som_sensitivity(
  scenarios,
  cross_models = c("kmeans", "ward"),
  cross_model_control = list(),
  consensus_method = "auto",
  max_coassignment_n = 5000L,
  max_pairwise_comparisons = 1000000L,
  sample_profiles = TRUE,
  keep_workflows = TRUE,
  fail_fast = FALSE
)
```

## Arguments

- scenarios:

  A named list of workflow argument lists.

- cross_models:

  Reference methods used in every scenario.

- cross_model_control:

  Named reference-model controls passed to every scenario through
  [`run_som_workflow()`](https://shaowen-ye.github.io/SOMevidence/reference/run_som_workflow.md).

- consensus_method:

  Consensus method used in every scenario.

- max_coassignment_n:

  Dense co-assignment limit.

- max_pairwise_comparisons:

  Pairwise partition-comparison budget passed to each scenario.

- sample_profiles:

  Whether to compute sample-level membership-set contrasts. Set to
  `FALSE` when the number of scenarios and samples would make the
  pairwise long table unnecessarily large.

- keep_workflows:

  Whether to retain full workflow objects.

- fail_fast:

  Whether to stop at the first scenario failure.

## Value

A `som_sensitivity` object with tidy evidence tables, shared-sample
comparisons between scenario consensus partitions, sample-level
membership-set comparisons, scenario failures and optionally full
workflows. Scenario agreement uses only samples assigned by at least two
ensemble members in both scenarios, and at least two jointly evaluable
samples are required before co-membership can be compared. For each
focal sample, `membership_jaccard_shared` conditions on the jointly
evaluable sample universe and isolates repartitioning, whereas
`membership_jaccard_all` also includes eligible members unique to either
scenario and therefore combines coverage and repartitioning effects.
Neither requires aligning arbitrary cluster labels. `sample_summary`
reports both distributions and distinguishes coverage of all planned
contrasts from coverage conditional on globally comparable contrasts.
These are sensitivity diagnostics, not confidence probabilities.
Comparisons across different data objects require supplied identifiers
or explicit row names; unsafe default identifiers, failed scenarios,
unavailable consensus and absent common `k` values remain visible
through `comparison_status` rather than being silently omitted. Legacy
objects without identifier provenance must be reconstructed with an
explicit `id` argument before cross-object comparison. Candidate `k`
values are read exactly from each scenario or its specification;
malformed values are not coerced. A failed scenario without a valid
candidate set remains visible with `k = NA` but cannot contribute to a
candidate-specific contrast denominator.

## Lifecycle

Experimental. The scenario-list interface may change after independent
usability testing. Individual workflow functions and their evidence
tables are the candidate-stable interfaces.
