# List governed tutorial and candidate open-data sources

The registry records provenance, version, licence, implementation status
and the intended role of each external dataset. Only records marked
`implemented` are used by a current package tutorial. Candidate records
are governed prospects, not claims of completed package validation. The
function returns metadata only; it never downloads data or changes the
user's files.

## Usage

``` r
open_data_registry(role = NULL, domain = NULL)
```

## Arguments

- role:

  Optional role used to filter the registry. Matching is exact and
  case-sensitive; unmatched values return no matching rows.

- domain:

  Optional scientific domain used to filter the registry. Matching is
  exact and case-sensitive; unmatched values return no matching rows.

## Value

A data frame with one row per governed external dataset.

## Examples

``` r
open_data_registry(role = "quickstart")
#>                id    domain       role
#> 1 palmer_penguins evolution quickstart
#>                                          title                    doi
#> 1 Palmer Archipelago (Antarctica) Penguin Data 10.5281/zenodo.3960218
#>                                                                                                                                    source_dois
#> 1 10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f|10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689|10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e
#>                                             version license
#> 1 palmerpenguins 0.1.1; citation DOI archives 0.1.0 CC0-1.0
#>                                      landing_url
#> 1 https://allisonhorst.github.io/palmerpenguins/
#>                                         redistribution
#> 1 access through the optional palmerpenguins R package
#>                                        planned_use      status accessed_date
#> 1 real-data morphology and post hoc label tutorial implemented    2026-08-11
```
