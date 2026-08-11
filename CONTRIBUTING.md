# Contributing to SOMevidence

Thank you for considering a contribution. `SOMevidence` treats software
interfaces and returned evidence structures as part of scientific
reproducibility, so changes should be focused, tested, and explicit
about their analytical consequences.

## Before opening a pull request

1.  Search the [issue
    tracker](https://github.com/shaowen-ye/SOMevidence/issues) for
    related work. Open an issue first for a new public function, a
    change to a stable default, or a change to a returned object.
2.  Create a branch from the current development branch. Do not modify
    or move a released tag.
3.  Keep generated, confidential, or non-redistributable research data
    out of the repository. A new external dataset needs a stable landing
    page, licence, version, citation, and a reproducible preparation
    record.
4.  Preserve inferential boundaries: agreement is not accuracy, internal
    validity is not ecological truth, and transfer evidence is distinct
    from analysis-scope stability.

## Development checks

Install the development dependencies listed in `DESCRIPTION`, then run:

``` r

roxygen2::roxygenise()
testthat::test_local()
lintr::lint_package()
covr::package_coverage(type = "tests")
```

Then build the source archive and check that archive, rather than
checking the working directory:

``` sh
R CMD build --compact-vignettes=both .
R CMD check --as-cran SOMevidence_*.tar.gz
```

The continuous-integration workflows repeat checks across current Linux,
macOS, and Windows runners, R-devel, R-oldrel, and the minimum declared
R version. New behavior should include focused tests; coverage is a
guardrail, not a substitute for tests of scientific invariants and
failure paths.

## API and documentation

- Follow the existing base-R style and keep user-facing names in
  `snake_case`.
- Document every exported argument, return value, warning, and failure
  policy.
- Update the machine-readable API and object contracts when a public
  interface changes.
- Add a `NEWS.md` entry under the unreleased version.
- Use a runnable example or vignette when users need more than the
  reference page to apply a feature safely.
- Do not change a stable default merely to improve one demonstration
  dataset.

## Pull-request description

State the user problem, the analytical behavior before and after the
change, tests added, documentation changed, and any compatibility
implications. Keep unrelated formatting or refactoring out of the same
pull request.

By contributing, you agree that your contribution will be distributed
under the package’s GPL-3.0-or-later license.
