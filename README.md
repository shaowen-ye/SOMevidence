# SOMevidence

`SOMevidence` is an R package for evaluating when classifications derived from
self-organizing maps (SOMs) are reproducible and scientifically defensible. It
is designed for ecological, environmental, and evolutionary data, where
sampling groups, monitoring domains, repeated observations, multiple data
layers, and incomplete transferability are common.

The package uses `kohonen` as its SOM training backend. It adds an
evidence-oriented workflow around training rather than reimplementing the SOM
algorithm.

## Key features

- leakage-safe preprocessing estimated separately within each analysis split;
- row-, group-, block-, and domain-aware resampling designs;
- reproducible ensembles across map grids, random seeds, and resamples;
- separate diagnostics for representation quality and hard-partition
  stability;
- consensus support, assignment entropy, clusterwise Jaccard, ARI, and AMI;
- controlled triangulation with K-means, Ward.D2, and Gaussian mixture models;
- held-domain mapping and sensitivity analyses;
- explicit decision gates that allow an insufficient-evidence outcome;
- editable analytical figures and an optional Shiny interface that exports
  reproducible R code and configuration.

`SOMevidence` does not select a universal best map or force ecological class
names. It keeps representation, partition, cross-model, external-label, and
transfer evidence distinct so that each claim can be assessed on its own
terms.

## Installation

Install the current release from R-universe:

```r
options(repos = c(
  shaowen_ye = "https://shaowen-ye.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
install.packages("SOMevidence")
```

The development version can be installed from GitHub:

```r
# install.packages("pak")
pak::pak("shaowen-ye/SOMevidence")
```

## Minimal example

```r
library(SOMevidence)

dat <- simulate_som_scenario("clusters", n = 180, p = 6, seed = 1)
splits <- som_resamples(dat, method = "subsample", repeats = 10, seed = 2)
spec <- som_spec(
  grids = list(c(5, 4), c(7, 5)),
  seeds = 3:7,
  rlen = 500,
  k = 2:6
)

result <- run_som_workflow(
  dat,
  spec,
  splits,
  cross_models = c("kmeans", "ward")
)

result$audit$grid_summary
result$partitions$stability
result$consensus$k3$cluster_summary
result$cross_comparison$summary
```

The workflow returns evidence for every requested candidate rather than one
automatically selected number of classes. Gaussian mixture-model comparison is
available when the suggested package `mclust` is installed.

## Documentation and data

Four vignettes introduce the core workflow, API lifecycle, design-aware
resampling and transfer, and evidence visualization. The package also includes
a registry of openly available demonstration datasets; external datasets are
not downloaded during installation, examples, or package checks.

This public repository contains only package source, tests, documentation, and
machine-readable API metadata. Package authorship and citation information are
available through `citation("SOMevidence")`.

## License

GPL (>= 3)
