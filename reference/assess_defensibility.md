# Assess partition defensibility without forcing a verdict

In addition to analyst-defined thresholds, the assessment checks that
every source partition and the consensus retain the requested `k`, and
that all requested cross-model fits succeeded. Missing audit evidence
produces an uncertain result rather than being treated as a pass.

## Usage

``` r
assess_defensibility(
  audit,
  partitions = NULL,
  k = NULL,
  gate = NULL,
  consensus = NULL,
  cross_model = NULL
)
```

## Arguments

- audit:

  A `som_audit` object.

- partitions:

  Optional `som_partitions` object. At least one partition- derived
  evidence object is required for a `"supported"` decision.

- k:

  Candidate number of clusters when partition stability is assessed.

- gate:

  Optional analyst-defined `som_gate`. Without one the status is
  `"not_assessed"`.

- consensus:

  Optional `som_consensus` object for the same `k`.

- cross_model:

  Optional `som_cross_comparison` object.

## Value

A `som_defensibility` object containing evidence and reasons.

## Details

A `"supported"` status means only that the analyst-specified evidence
requirements and structural completeness checks were met for the stated
data, analysis design, preprocessing, model budget and candidate `k`. It
does not demonstrate that discrete ecological types exist, identify a
causal mechanism, or establish ecological truth.

A representation audit alone cannot support a hard partition. A
supported decision also requires traceable comparative partition
evidence and at least one prespecified partition-quality requirement.
Missing evidence or a representation-only gate produces an `"uncertain"`
result.
