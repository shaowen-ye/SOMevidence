# API lifecycle and reproducibility contracts

## Why the package has contracts

`SOMevidence` treats software interfaces and evidence structures as part
of reproducibility. A changed argument, default, or returned field can
alter an analysis even when a function retains the same name. The
package therefore ships machine-readable contracts for both exported
functions and returned objects.

The version 1.0.0 contracts remain the structural compatibility
baseline. The current 1.1.0 contracts describe additive interfaces and
scientific corrections that make previously implicit safeguards
observable. Structural compatibility does not imply that an analysis
which omits version-sensitive defaults will be numerically identical
across releases.

``` r

contract_path <- function(name) {
  system.file("extdata", name, package = "SOMevidence")
}

baseline_api <- utils::read.csv(
  contract_path("api-contract-v1.0.0.csv"),
  check.names = FALSE
)
api_contract <- utils::read.csv(
  contract_path("api-contract-v1.1.0.csv"),
  check.names = FALSE
)
object_contract <- utils::read.csv(
  contract_path("object-contract-v1.1.0.csv"),
  check.names = FALSE
)

c(baseline_exports = nrow(baseline_api), current_exports = nrow(api_contract))
#> baseline_exports  current_exports 
#>               23               23
table(api_contract$lifecycle)
#> 
#> experimental       stable 
#>            3           20
head(object_contract[c("class", "lifecycle")])
#>            class lifecycle
#> 1       som_data    stable
#> 2 som_preprocess    stable
#> 3  som_resamples    stable
#> 4       som_spec    stable
#> 5   som_ensemble    stable
#> 6      som_audit    stable
```

Automated tests require all baseline arguments to remain present and in
their original order. They also compare the current namespace, complete
function formals, key defaults, return classes, object components, and
selected evidence table schemas with the 1.1.0 contracts.

## What changed in the 1.1.0 contract

The current contract makes several correctness safeguards explicit:

- named multi-layer inputs are aligned by sample identity or rejected;
- resampling objects retain ordered sample identifiers and unique split
  IDs;
- Ward.D2 training labels remain the
  [`cutree()`](https://rdrr.io/r/stats/cutree.html) partition, whereas
  new rows use an explicitly named nearest-centroid projection rule;
- GMM initialization uses a recorded deterministic seed;
- partition records retain the number of sample-level classes actually
  observed, and incomplete `k` partitions cannot enter consensus;
- aligned voting can propagate labels along a connected overlap path and
  reports consensus-label coverage separately from assignment coverage;
- pairwise auditing has a user-visible computation budget;
- transfer evidence reports mapping counts and coverage;
- training-constant variables are excluded from held-out distance; and
- cross-model gates use method-specific evidence rather than a pooled
  median.

Version 1.1.0 also changes the default reference set from K-means,
Ward.D2 and GMM to K-means and Ward.D2. This avoids making an optional
dependency and its additional model-selection step part of every default
run. To reproduce the 1.0 default reference set, request it explicitly:

``` r

cross_models <- c("kmeans", "ward", "gmm")
```

This default change can alter fitted evidence and is therefore recorded
in the versioned contract and `NEWS.md`. Reproducible analyses should
pin the package version and specify the reference methods rather than
relying on defaults.

The top-level workflow now records requested candidate values and
lightweight software provenance. Its summary reports model, consensus,
warning, and failure counts without requiring users to traverse nested
lists. Compact sensitivity results retain model-stage failures and
warnings, and new-data mapping objects retain structured warnings.

These are software and evidential safeguards. They do not turn internal
validity, cross-model agreement, or transfer diagnostics into ecological
truth. The repository’s [validation-scope
statement](https://github.com/shaowen-ye/SOMevidence/blob/main/VALIDATION_SCOPE.md)
separates software verification, design-conditioned reproducibility,
worked examples, and future external validation.

## Stable and experimental interfaces

Stable functions cover the data contract, preprocessing, sampling
design, ensemble fitting, evidence audits, transfer, and conditional
decision gates. The names and positional prefix of stable 1.0.0
arguments remain supported. Removing or reordering those arguments, or
withdrawing a documented object guarantee, requires a new major package
version. Analysis-affecting default changes must be explicit in a
versioned contract and release notes; publication workflows should still
record every consequential setting directly.

Three interfaces remain experimental:

``` r

api_contract[api_contract$lifecycle == "experimental",
             c("function", "return_class", "notes")]
#>                 function    return_class
#> 10        launch_som_app    shiny.appobj
#> 16   run_som_sensitivity som_sensitivity
#> 18 simulate_som_scenario        som_data
#>                                                                                                   notes
#> 10 Returns an optional Shiny application; the exported R script remains the executable analysis record.
#> 16              Preserves scenario-level and model-stage failures even when full workflows are omitted.
#> 18                                   Research and teaching utility whose scenario catalogue may expand.
```

[`launch_som_app()`](https://shaowen-ye.github.io/SOMevidence/reference/launch_som_app.md)
returns a `shiny.appobj`. It can be printed in an interactive session or
passed to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), but it
does not return an analysis result. `SOMevidence` adds no telemetry. In
a local session, a selected file remains on the local computer; in a
remotely deployed app, Shiny transfers it to that host. Its YAML export
is a configuration snapshot. The exported R script can be rerun, but it
becomes part of a reproducible record only when retained with the exact
input, software versions, warnings, failures, and results. The
sensitivity and simulation interfaces also remain experimental because
their scenario descriptions may evolve after broader use.

## Object boundaries are scientific boundaries

The returned classes keep distinct questions apart:

- `som_audit` concerns representation quality;
- `som_partitions` and `som_consensus` concern analysis-scope
  hard-partition stability;
- `som_cross_comparison` concerns algorithmic agreement;
- `som_transfer_audit` concerns held-domain behaviour;
- `som_external_assessment` concerns post hoc agreement with labels
  excluded from training; and
- `som_defensibility` records an analyst-specified conditional decision.

No object is a certificate of ecological truth. Cross-model agreement is
not a leaderboard, external-label agreement is not classification
accuracy, missing evidence remains visible, and insufficient evidence
may lead to abstention. A `supported` decision means only that the
stated analyst-specified evidence requirements and structural checks
passed for that workflow. It does not prove that discrete ecological
types exist.

A continuous gradient illustrates the boundary: imposing a fixed `k` can
yield similar partitions across resamples, starts, or algorithms even
though the underlying structure contains no discrete classes. Stability
and cross-model agreement therefore remain conditional evidence, not a
test of discreteness.

## Reporting an analysis

Archive the package version, session information, analysis script,
sampling design, and gate settings with every analysis:

``` r

packageVersion("SOMevidence")
#> [1] '1.1.1'
sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.60         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
#>  [9] rmarkdown_2.31    lifecycle_1.0.5   cli_3.6.6         sass_0.4.10      
#> [13] pkgdown_2.2.1     textshaping_1.0.5 jquerylib_0.1.4   systemfonts_1.3.2
#> [17] compiler_4.6.1    tools_4.6.1       ragg_1.5.2        bslib_0.12.0     
#> [21] evaluate_1.0.5    yaml_2.3.12       otel_0.2.0        jsonlite_2.0.0   
#> [25] rlang_1.3.0       fs_2.1.0
```

For work intended to support a publication, also retain the exact data
provenance, stable sample identifiers, preprocessing specification,
random seeds, model failures, warnings, model budget, and API contract
version used by the analysis.
