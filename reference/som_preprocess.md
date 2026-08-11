# Define leakage-safe SOM preprocessing

Define leakage-safe SOM preprocessing

## Usage

``` r
som_preprocess(
  transform = "identity",
  center = TRUE,
  scale = TRUE,
  zero_replacement = NULL
)
```

## Arguments

- transform:

  One of `"identity"`, `"log"`, `"log1p"`, `"sqrt"`, `"hellinger"` or
  `"clr"`. `"log"` denotes the natural logarithm. A vector can assign
  `"identity"`, `"log1p"`, `"log"` or `"sqrt"` by column; name that
  vector to protect against column-order changes. Hellinger and CLR are
  whole-matrix transformations and must be specified alone.

- center:

  Whether to subtract training-set column means.

- scale:

  Whether to divide by training-set column standard deviations.

- zero_replacement:

  Positive replacement required when `transform = "clr"` and zeros are
  present.

## Value

A preprocessing specification of class `som_preprocess`.

## Examples

``` r
som_preprocess(transform = c("log", "identity"))
#> $transform
#> [1] "log"      "identity"
#> 
#> $center
#> [1] TRUE
#> 
#> $scale
#> [1] TRUE
#> 
#> $zero_replacement
#> NULL
#> 
#> attr(,"class")
#> [1] "som_preprocess"
#> attr(,"som_contract_version")
#> [1] "1.2.0"
```
