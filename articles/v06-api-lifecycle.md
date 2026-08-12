# Reproducibility and API versions

## Why versions matter

An analysis can change when a function argument, default or returned
field changes. The function name may stay the same. `SOMevidence`
therefore keeps machine-readable records of its public functions and
result objects.

These records serve two purposes:

- package tests can detect an unintended interface change; and
- analysts can see which structure belongs to a released version.

They do not guarantee identical numerical results when an analysis
relies on defaults that later change. For a published workflow, record
all important settings directly.

## Inspect the current contracts

``` r

contract_path <- function(name) {
  candidates <- c(
    file.path("inst", "extdata", name),
    file.path("..", "inst", "extdata", name),
    system.file("extdata", name, package = "SOMevidence")
  )
  available <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(available)) {
    stop("The requested API contract is not available: ", name)
  }
  available[[1L]]
}

api_contract <- utils::read.csv(
  contract_path("api-contract-v1.2.0.csv"),
  check.names = FALSE
)

api_formals <- utils::read.csv(
  contract_path("api-formals-v1.2.0.csv"),
  check.names = FALSE
)

object_contract <- utils::read.csv(
  contract_path("object-contract-v1.2.0.csv"),
  check.names = FALSE
)

table(api_contract$lifecycle)
#> 
#> experimental       stable 
#>            4           21
head(api_formals)
#>                   function
#> 1     assess_defensibility
#> 2                audit_som
#> 3 audit_som_representation
#> 4           audit_transfer
#> 5     compare_cross_models
#> 6            consensus_som
#>                                                                          defaults
#> 1                                             <required>|NULL|NULL|NULL|NULL|NULL
#> 2                                                                      <required>
#> 3          <required>|c("analysis", "assessment")|NULL|NULL|10000L|1000000L|FALSE
#> 4                                                                      <required>
#> 5                        <required>|<required>|c("analysis", "assessment", "all")
#> 6 <required>|<required>|"average"|c("auto", "coassignment", "aligned_vote")|5000L
head(object_contract[c("class", "lifecycle")])
#>            class lifecycle
#> 1       som_data    stable
#> 2 som_preprocess    stable
#> 3  som_resamples    stable
#> 4       som_spec    stable
#> 5   som_ensemble    stable
#> 6      som_audit    stable
```

The API contract lists exported functions and their lifecycle. The
formals contract records arguments and defaults. The object contract
records the fields that downstream code may expect from public results.

The version 1.2.0 object contract also gives each result a
`som_contract_version`. It can align external labels by sample ID and
keeps missing, excluded and insufficiently replicated records separate.

Release history and changes to defaults belong in `NEWS.md`. This
tutorial focuses on how to use the contracts rather than repeating the
changelog.

## Stable and experimental functions

Most functions used for data preparation, resampling, SOM fitting,
audits and transfer are stable. Their established arguments and
documented result fields are maintained within a major version.

Some interfaces remain experimental:

``` r

api_contract[
  api_contract$lifecycle == "experimental",
  c("function", "return_class", "notes")
]
#>                    function             return_class
#> 3  audit_som_representation som_representation_audit
#> 11           launch_som_app             shiny.appobj
#> 17      run_som_sensitivity          som_sensitivity
#> 19    simulate_som_scenario                 som_data
#>                                                                                                                       notes
#> 3  Reports exact cross-fit topology reproducibility under explicit computation budgets without ranking or selecting models.
#> 11                     Returns an optional Shiny application; the exported R script remains the executable analysis record.
#> 17                                  Preserves scenario-level and model-stage failures even when full workflows are omitted.
#> 19                                                       Research and teaching utility whose scenario catalogue may expand.
```

Experimental does not mean untested. It means that feedback from wider
use may still change the interface or the way results are organized. In
version 1.2.0, this applies to the Shiny app, simulation helpers,
sensitivity workflow and continuous cross-fit representation audit.

The Shiny app can export a runnable R script. Its YAML file is a
configuration snapshot, not an importable analysis file. Keep the script
with the data, software versions, warnings and results.

## Each result answers one question

The package uses separate object classes so that different types of
evidence are not confused:

| Object | Question |
|----|----|
| `som_audit` | How well did the maps represent the analysis data? |
| `som_representation_audit` | How similar was sample topology across selected maps? |
| `som_partitions` | How reproducible were candidate hard partitions? |
| `som_consensus` | Which sample assignments were repeatedly supported? |
| `som_cross_comparison` | Did reference methods produce similar partitions? |
| `som_transfer_audit` | How did held-out domains map to trained SOMs? |
| `som_external_assessment` | How did a consensus agree with labels excluded from training? |
| `som_defensibility` | Did an analyst’s stated evidence requirements pass? |

No object is a certificate of ecological truth. A `supported` decision
means that the stated checks passed for that workflow. It does not prove
that nature contains the displayed classes.

## Open an older saved object

[`upgrade_som_object()`](https://shaowen-ye.github.io/SOMevidence/reference/upgrade_som_object.md)
can add fields when the migration is deterministic and the older object
already contains the required evidence.

``` r

old_result <- readRDS("saved-som-result.rds")
current_result <- upgrade_som_object(old_result)
```

If the older result lacks required evidence, refit the analysis. Do not
invent missing fields or copy them from another run.

## Record a publication analysis

Save:

- the package and R versions;
- input provenance and stable sample IDs;
- preprocessing and resampling choices;
- grids, seeds, iterations and candidate `k` values;
- reference methods and any decision rules; and
- complete warnings, failures and evidence tables.

``` r

packageVersion("SOMevidence")
#> [1] '1.2.1'
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

The contract tells software what a result contains. The analysis record
tells readers how that result was produced. A reproducible study needs
both.
