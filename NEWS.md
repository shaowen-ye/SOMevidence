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
