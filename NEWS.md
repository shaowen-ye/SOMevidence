# SOMevidence 1.1.0

## Scientific correctness and reproducibility

- Aligns named layers by validated sample identity and rejects ambiguous or
  mismatched multi-layer inputs before training.
- Binds resampling objects to ordered sample identifiers, validates unique
  split identifiers, and prevents reuse with a different sample universe or row
  order.
- Preserves the Ward.D2 `cutree()` partition for training observations and
  labels nearest-centroid assignment explicitly as an out-of-sample projection
  rule.
- Adds deterministic, fit-specific GMM seeds, including when `mclust` uses
  stochastic subsetting for large datasets.
- Propagates aligned-vote labels through connected partition-overlap paths and
  distinguishes assignment coverage from identifiable consensus-label
  coverage.
- Records the number of sample-level clusters actually represented, prevents
  incomplete `k` partitions from entering consensus, and treats incomplete
  cluster evidence as unevaluable in decision gates.
- Adds an explicit pairwise-comparison budget so large ensemble audits fail
  before generating an unintended quadratic workload, and preallocates the
  retained pairwise table instead of accumulating one data frame per contrast.
- Computes training topographic error during fitting and discards duplicated
  full processed matrices from each fit record to reduce ensemble memory use.
- Reports training and assessment mapping coverage in transfer audits and
  excludes training-constant variables from held-out distance calculations.
- Evaluates cross-model gate requirements by reference method rather than by a
  pooled comparison dominated by methods with more fitted records.

## Workflows and interface

- The default cross-model references are K-means and Ward.D2. GMM is used only
  when `"gmm"` is requested explicitly and the suggested package `mclust` is
  installed.
- Adds concise workflow summaries, requested-`k` accounting, lightweight
  provenance, and retained model-stage warnings and failures in compact
  sensitivity results.
- Hardens the optional Shiny interface with data preflight, metadata separation,
  configuration validation, visible diagnostics, and versioned script and YAML
  exports. `launch_som_app()` returns a `shiny.appobj`.

## Documentation and release engineering

- Added a fully executable real-data tutorial using Palmer penguin morphology,
  with species retained exclusively for post hoc agreement assessment.
- Added automated cross-platform R package checks, a minimum supported R
  version check, test-coverage enforcement, linting, and pkgdown deployment.
- Added a public documentation site configuration, contributor guidance,
  support guidance, and machine-readable citation metadata.
- Corrected and clarified the provenance, licence, version, and implementation
  status of sources in the open-data registry.
- Shortened the README quick start and clarified the package's evidential scope,
  optional dependencies, support channels, and reproducibility expectations.

# SOMevidence 1.0.1

- Updated the package maintainer email address. No API or analytical behaviour
  changed in this patch release.

# SOMevidence 1.0.0

## Data design and preprocessing

- Added explicit data, preprocessing, resampling, and SOM specification
  objects.
- Added leakage-safe transformations and layer normalization estimated within
  each analysis split.
- Added row-, group-, block-, leave-domain-out, and custom resampling designs.

## SOM ensembles and evidence

- Added reproducible single- and multi-layer SOM ensembles across map grids,
  random seeds, and resamples.
- Added quantization error, topographic error, empty-unit rate, and structured
  model-warning and failure records.
- Added candidate-partition stability, ARI, AMI, clusterwise Jaccard,
  membership support, assignment entropy, and scalable consensus analyses.
- Added explicit defensibility gates without a composite score, universal
  threshold, forced class naming, or automatic selection of the number of
  classes.

## Triangulation, transfer, and sensitivity

- Added controlled K-means, Ward.D2, and Gaussian mixture-model comparisons
  using common splits and preprocessing.
- Added held-domain and new-data mapping diagnostics based only on
  training-derived preprocessing.
- Added prespecified sensitivity workflows and cross-scenario comparisons.
- Added post hoc external-label assessment that reports agreement rather than
  classification accuracy.

## Visualization and reproducibility

- Added editable component, occupancy, neighbour-distance, stability,
  consensus, cross-model, transfer, and sensitivity plots.
- Added an optional Shiny interface with reproducible R-script and YAML export.
- Added four executable vignettes, automated tests, package citation, and
  machine-readable contracts for exported functions and returned objects.
