# SOMevidence

[![R-universe
version](https://shaowen-ye.r-universe.dev/badges/SOMevidence)](https://shaowen-ye.r-universe.dev/SOMevidence)
[![R-CMD-check](https://github.com/shaowen-ye/SOMevidence/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shaowen-ye/SOMevidence/actions/workflows/R-CMD-check.yaml)
[![Test
coverage](https://github.com/shaowen-ye/SOMevidence/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/shaowen-ye/SOMevidence/actions/workflows/test-coverage.yaml)
[![License:
GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue.svg)](https://shaowen-ye.github.io/SOMevidence/LICENSE)

`SOMevidence` is an R package for asking when classifications derived
from self-organizing maps (SOMs) are reproducible and scientifically
defensible. It is intended for ecological, environmental, and
evolutionary data in which sampling groups, monitoring domains, repeated
observations, multiple data layers, and incomplete transferability are
common.

The package uses [`kohonen`](https://cran.r-project.org/package=kohonen)
as its SOM training backend. Its contribution is an evidence workflow
around SOM training: explicit data design, leakage-safe preprocessing,
controlled ensembles, partition stability, cross-model agreement,
transfer diagnostics, and conditional decisions that can abstain when
evidence is insufficient.

## What the package separates

- representation quality from hard-partition stability;
- analysis-scope reproducibility from held-domain transfer;
- cross-model agreement from ecological validation;
- post hoc agreement with external labels from classification accuracy;
  and
- evidence reporting from analyst-defined decision thresholds.

This separation is deliberate. `SOMevidence` does not select a universal
best map, force ecological class names, or turn unsupervised agreement
into evidence of ecological truth.

## Main capabilities

- leakage-safe preprocessing fitted separately within each analysis
  split;
- row-, group-, block-, and domain-aware resampling designs;
- reproducible ensembles across map grids, random seeds, and resamples;
- quantization, topology, occupancy, consensus, entropy, Jaccard, ARI,
  and AMI diagnostics;
- controlled triangulation with K-means, Ward.D2, and, when requested,
  Gaussian mixture models;
- held-domain and later-batch mapping without refitting preprocessing on
  the assessment data;
- explicit defensibility gates with an `"abstain"` outcome; and
- analytical graphics plus an optional Shiny interface that exports R
  code and configuration.

## Installation

Install the current release from R-universe:

``` r

options(repos = c(
  shaowen_ye = "https://shaowen-ye.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
install.packages("SOMevidence")
```

Install the source currently on GitHub with
[`pak`](https://pak.r-lib.org/):

``` r

# install.packages("pak")
pak::pak("shaowen-ye/SOMevidence")
```

## Quick start

The following small run varies the resample, map grid, and random start.
Its settings are deliberately modest for an interactive example; a
scientific analysis should justify a larger, prespecified model budget.

``` r

library(SOMevidence)

dat <- simulate_som_scenario("clusters", n = 120, p = 6, seed = 1)
splits <- som_resamples(
  dat,
  method = "subsample",
  repeats = 5,
  prop = 0.9,
  seed = 2
)
spec <- som_spec(
  grids = list(c(4, 3), c(5, 4)),
  seeds = c(3, 4),
  rlen = 50,
  k = 2:4
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
summary(result)
result$consensus_failures
```

The workflow reports every requested candidate `k`; it does not silently
pick one. Gaussian mixture-model comparison is available when the
suggested package `mclust` is installed and `"gmm"` is requested
explicitly.

## Start from your own data

Separate predictors from sampling-design fields before constructing the
analysis object. Numeric site codes, coordinates, dates, depths, and
external class labels should not enter the SOM merely because they are
numeric. The placeholder path below belongs in a separate, controlled
analysis project; do not copy confidential study data into the package
source repository.

``` r

raw <- read.csv("data/your-analysis-ready-data.csv", check.names = FALSE)
predictors <- c("temperature", "oxygen", "conductivity", "total_nitrogen")

dat <- som_data(
  x = raw[predictors],
  id = raw$sample_id,
  group = raw$site_id,
  time = raw$sampling_date,
  domain = raw$survey_campaign
)

# Illustrative settings only: choose these from the sampling design and a
# prespecified computation budget for the study at hand.
splits <- som_resamples(
  dat,
  method = "group_subsample",
  repeats = 20,
  prop = 0.8,
  seed = 2026
)
spec <- som_spec(
  grids = list(c(7, 5), c(8, 6)),
  seeds = 1:10,
  rlen = 1000,
  k = 2:8
)
result <- run_som_workflow(
  dat,
  spec,
  splits,
  cross_models = c("kmeans", "ward")
)
summary(result)
```

Choose resampling from the observation hierarchy, not from convenience:

| Data structure                                     | Starting design    |
|----------------------------------------------------|--------------------|
| Independent observations                           | `subsample`        |
| Repeated observations within sites or subjects     | `group_subsample`  |
| Prespecified spatial or temporal blocks            | `block_subsample`  |
| Transfer across campaigns, regions, or instruments | `leave_domain_out` |
| A protocol-defined split                           | `custom`           |

This table is a starting point rather than an automatic rule.
Dependence, sample size, domain coverage, transformations, missingness,
and the scientific question still require study-specific decisions.
Inspect the workflow summary, all failure and warning tables, and the
separate evidence streams before interpreting any partition.

## Optional interactive interface

[`launch_som_app()`](https://shaowen-ye.github.io/SOMevidence/reference/launch_som_app.md)
returns a `shiny.appobj`, which can be printed in an interactive R
session or passed to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). The
application helps configure and inspect a workflow, but it does not
return an analysis object. The exported R script, together with its
configuration and input data, is the reproducible analysis record.

## Learn with real and simulated data

The [package website](https://shaowen-ye.github.io/SOMevidence/)
contains five executable tutorials:

- the core ensemble and evidence workflow;
- API lifecycle and reproducibility contracts;
- design-aware resampling and held-domain transfer;
- evidence visualization; and
- a real-data analysis of Palmer penguin morphology, with species labels
  held out until post hoc agreement assessment.

[`open_data_registry()`](https://shaowen-ye.github.io/SOMevidence/reference/open_data_registry.md)
lists the implemented tutorial source and additional curated candidate
datasets for ecological, environmental, and evolutionary examples.
Candidate datasets are not claimed to be package validation data and are
never downloaded during installation or package checks.

## Reproducible reporting

For a scientific analysis, retain at least:

- the `SOMevidence` version and
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html);
- the analysis-ready data provenance and stable sample identifiers;
- the preprocessing, resampling, SOM, and cross-model specifications;
- all seeds, model warnings, model failures, and decision-gate settings;
  and
- the exact script that produced the reported tables and figures.

Package-level checks reduce software errors, but they do not establish
that a sampling design, candidate `k`, ecological interpretation, or
threshold is appropriate for a particular study.

## Citation, support, and contribution

Use `citation("SOMevidence")` for the current citation. Questions and
bug reports are handled through [GitHub
Issues](https://github.com/shaowen-ye/SOMevidence/issues); please read
[SUPPORT.md](https://shaowen-ye.github.io/SOMevidence/SUPPORT.md) before
filing a report. Contributions are welcome under the process in
[CONTRIBUTING.md](https://shaowen-ye.github.io/SOMevidence/CONTRIBUTING.md).

## License

`SOMevidence` is distributed under the GNU General Public License
version 3 or later. See
[LICENSE.md](https://shaowen-ye.github.io/SOMevidence/LICENSE.md).
