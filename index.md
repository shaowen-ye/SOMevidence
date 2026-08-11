# SOMevidence

[![R-universe
version](https://shaowen-ye.r-universe.dev/badges/SOMevidence)](https://shaowen-ye.r-universe.dev/SOMevidence)
[![R-CMD-check](https://github.com/shaowen-ye/SOMevidence/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shaowen-ye/SOMevidence/actions/workflows/R-CMD-check.yaml)
[![Test
coverage](https://github.com/shaowen-ye/SOMevidence/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/shaowen-ye/SOMevidence/actions/workflows/test-coverage.yaml)
![License:
GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue.svg)

`SOMevidence` is an R package for auditing whether classifications
derived from self-organizing maps (SOMs) meet prespecified evidence
requirements under an explicit analysis design. It is intended for
ecological, environmental, and evolutionary data in which sampling
groups, monitoring domains, repeated observations, multiple data layers,
and incomplete transferability are common.

The package uses [`kohonen`](https://cran.r-project.org/package=kohonen)
as its SOM training backend. Its contribution is an evidence workflow
around SOM training: explicit data design, leakage-safe preprocessing,
controlled ensembles, partition stability, cross-model agreement,
transfer diagnostics, and conditional decisions that can abstain when
evidence is insufficient. The [validation
scope](https://github.com/shaowen-ye/SOMevidence/blob/main/VALIDATION_SCOPE.md)
distinguishes software checks, design-conditioned reproducibility,
worked examples, and external validation.

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
- explicit analyst-specified evidence gates with an `"abstain"` outcome;
  and
- analytical graphics plus an optional Shiny interface that exports R
  code and configuration.

## Metrics dictionary

Directions below are diagnostic tendencies within otherwise comparable
data, preprocessing, scopes, and model budgets. They are not universal
thresholds and should not be combined into an automatic leaderboard.

| Question | Metrics | Diagnostic direction | Required evidence | Does not establish |
|----|----|----|----|----|
| How well does a fitted map represent its analysis data? | Quantization error, topographic error, empty-unit rate | Generally lower within comparable fits; inspect trade-offs | Successful fits using the same variables, preprocessing, scope, and comparable grids | A best map, a true number of classes, or ecological validity |
| Is a fixed-`k` partition reproducible across the planned ensemble? | Pairwise ARI/AMI, clusterwise Jaccard, joint coverage, complete-partition rate | Agreement and coverage higher; every source partition retains `k` | At least two successful, comparable partitions with adequate shared observations | That discrete types exist, that `k` is optimal, or that labels are correct |
| How consistently is each sample assigned? | Membership support, assignment entropy, assignment, consensus-label, and replicated coverage | Support and coverage higher; entropy lower | An identifiable consensus with repeated assignments and explicit coverage | A posterior probability or probability that an assignment is correct |
| Do controlled reference algorithms agree with the SOM partition? | Method-specific SOM-to-reference ARI, method count, reference-fit success | Agreement higher, with all requested methods evaluable | Prespecified reference methods fitted on common splits and preprocessing | Accuracy, independence of evidence, or ecological validation |
| Does the representation map comparably to a held-out domain? | Assessment-to-analysis distance ratio, unoccupied-unit rate, mapping coverage | Ratio nearer 1, unoccupied rate lower, coverage higher; inspect all fits | Genuine held-domain rows mapped using training-derived preprocessing | Predictive accuracy, causal explanation of shift, or general transferability |
| Are conclusions sensitive to prespecified analysis choices? | Scenario ARI/AMI, shared- and all-members Jaccard, contrast coverage | Agreement and coverage higher across the stated scenarios | Stable sample IDs, prespecified scenarios, and adequate shared observations | Robustness to unexamined choices or a probability that membership is correct |
| Does a consensus agree with labels excluded from training? | External-label ARI/AMI, contingency and composition tables | Agreement higher among evaluable samples | Labels kept outside training and joined by stable sample identity | Classification accuracy, ecological ground truth, or causation |
| Did a candidate meet the analyst’s evidence requirements? | `supported`, `abstain`, `uncertain`, and the individual gate checks | All specified checks pass; failures abstain; unavailable evidence is uncertain | Prespecified thresholds and complete evidence from the same workflow | The existence of discrete ecological types or universal scientific defensibility |

SOMevidence calculates AMI with chance adjustment from the expected
mutual information and uses the arithmetic mean of the two partition
entropies as its normalizer. Software that uses a different AMI
normalization can return a different numerical value for the same pair
of partitions.

Representation-level checks alone cannot support a hard partition. A
supported decision requires traceable comparative partition evidence and
at least one prespecified partition-quality requirement. Otherwise the
missing requirement is reported as unavailable and
[`assess_defensibility()`](https://shaowen-ye.github.io/SOMevidence/reference/assess_defensibility.md)
returns `"uncertain"`.

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
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
`SOMevidence` adds no telemetry. When the app runs in a local R session,
a selected CSV remains on the local computer. If the app is deployed on
a remote Shiny host, selecting a file transfers it to that host; users
should follow the host operator’s access and data-handling controls. The
application helps configure and inspect a workflow, but it does not
return an analysis object. The YAML export is a configuration snapshot.
The exported R script can be rerun, but neither export replaces the
exact input data, package versions, warnings, failures, and results
required for a reproducible analysis record.

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
