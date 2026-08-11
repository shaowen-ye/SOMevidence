# SOMevidence (development version)

## Documentation and release evidence

- Preserves explicit row identity when only some row names resemble generated
  `sample_<integer>` identifiers; only fully generic sequences are now treated
  as generated when `id` is omitted.
- Replaces a stale GUI version reference with a version-neutral statement of
  the current configuration-import boundary.
- Clarifies the conditional scope of the grouped-simulation correlation,
  domain-dependent missingness control and the combined range-extrapolation
  and additive-shift mechanism in the teaching gradient scenario.
- Records a subsequent R-universe distribution refresh without treating a
  generated source-package hash as the immutable software-source identity.

# SOMevidence 1.2.0

## Identity-safe evidence and durable results

- Aligns supplied external labels by validated sample identity, while retaining
  an explicitly warned positional compatibility path for unnamed legacy input.
- Reports mutually exclusive record-level accounting for label availability,
  exclusions, consensus coverage and replication support.
- Adds versioned structural contracts to public result objects and a
  deterministic `upgrade_som_object()` path for migrations that can be made
  without refitting models or reconstructing scientific evidence.

## Continuous-representation reproducibility

- Adds experimental `audit_som_representation()` diagnostics for within-scope
  quantization, topology, occupancy and mapping coverage across SOM fits.
- Compares prespecified fit pairs through shortest-hop grid distances and,
  when requested, tie-preserving local-neighbourhood overlap.
- Uses exact calculations under explicit fit-pair and sample-pair budgets and
  returns descriptive evidence without a score, ranking or automatic model
  selection rule.
- Preserves the pairwise-comparison budget in partition results even when all
  requested SOM fits fail, keeping successful and unsuccessful runs under one
  result contract.

# SOMevidence 1.1.3

## Performance and regression assurance

- Reduces task metadata retained during ensemble fitting, particularly for
  large grid-by-seed-by-resample budgets, without changing fit identifiers,
  task order, warnings, failures or reproducible random-number handling.
- Avoids calculating topographic-distance matrices for rows outside the
  requested audit domain.
- Reuses contingency tables when both adjusted Rand and adjusted mutual
  information are required, and reuses each Ward.D2 tree across candidate
  cluster counts within a cross-model analysis split.
- Accelerates complete-data co-assignment matrices and overlap-based label
  propagation while preserving the established consensus outputs and
  diagnostic records.
- Adds frozen version 1.1.2 reference kernels and exact release-oracle tests
  for numerical results, object structure, record order, warnings, failures
  and random-number state. No exported function, argument or default changes
  in this patch release.
- Clarifies that AMI uses arithmetic-mean entropy normalization so comparisons
  with software using a different AMI normalization remain interpretable.

# SOMevidence 1.1.2

## Scientific decision guards

- Prevents representation-only evidence gates from returning `supported` for
  a hard partition. A supported decision now requires traceable comparative
  partition evidence and at least one prespecified partition-quality rule;
  unavailable evidence produces an `uncertain` decision.
- Verifies from explicit partition-method provenance and candidate records that
  partitions, consensus and cross-model comparisons supplied to one decision
  derive from the same partitioning pipeline.
- Prevents fit-level Pareto selection from silently comparing fits trained on
  distinct analysis rows. For compatibility, the established audit interface
  returns an explicitly warned and annotated exploratory frontier; scientific
  selection should use comparable configuration summaries instead.
- Blocks the experimental GUI before fitting when selected predictors contain
  missing values, because the 1.1.x interface does not expose a missing-
  distance policy.
- Checks preprocessing and layer-weight prerequisites within every GUI analysis
  split, and blocks a requested cross-model analysis when no split is eligible.
- Records and warns about repeated analysis sets in resampling objects; the GUI
  blocks designs that would overweight duplicate data perturbations.
- Describes the current representation layer as diagnostics rather than
  claiming cross-fit reproducibility metrics that are not yet implemented.
- Makes CI success conditional on a complete `R CMD check` log, disables the
  network-dependent remote incoming query in the routine matrix, and adds PDF
  manual and manually triggered remote CRAN preflight checks.
- Pins third-party GitHub Actions to immutable commits and adds explicit job
  timeouts.

# SOMevidence 1.1.1

## Computation and reporting

- Checks the conservative upper bound on requested pairwise partition
  comparisons before any SOM is fitted, so an over-budget workflow stops before
  spending its training budget.
- Replaces quadratic sample-by-sample equivalence checks for degenerate ARI and
  AMI cases with contingency-table checks, and prevents integer overflow in AMI
  calculations for large sample counts.
- Preserves the consensus `status` values from version 1.1.0 and adds
  `computation_status` as `computed` or `not_computed`, separately from
  scientific gate decisions. It also summarizes consensus completeness and
  assignment coverage.
- Summarizes expected, successful and failed reference-model fits, warnings and
  success rates by method, including compatible fallbacks for older workflow
  objects.

## Interactive preflight

- Audits planned versus structurally feasible SOM fits from split sizes and map
  units, blocks designs for which every planned fit is structurally impossible,
  and flags partially feasible designs for review.
- Shows the planned pairwise-comparison count and blocks GUI runs that would
  exceed the same limit enforced by `run_som_workflow()`.
- Flags repeated analysis sets and checks split-specific preprocessing and
  sample-count prerequisites for requested cross-model references.
- Gives actionable CSV errors without exposing local paths, accepts files with
  an incomplete final line without relying on localized warning text, and
  improves singular and plural status messages.

## Scientific scope and documentation

- Clarifies that a `supported` decision means only that a candidate met
  analyst-specified evidence requirements; it does not demonstrate that
  discrete ecological types exist.
- Adds a central metrics dictionary and a validation-scope statement separating
  software verification, design-conditioned reproducibility, worked examples,
  and future ADEMP simulation and external validation.
- Documents that the Shiny interface adds no package telemetry, while clearly
  distinguishing local file handling from remotely hosted deployments; its
  YAML export is a configuration snapshot.
- Documents the exact, case-sensitive matching behavior of open-data registry
  filters while preserving version 1.1.0 handling of unmatched values.
- Adds a privacy-aware feature-request template.
- Adds a public local-validation record for the release source; external build
  identity remains a separate post-release check.

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
