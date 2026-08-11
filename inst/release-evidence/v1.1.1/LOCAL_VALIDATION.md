# Local validation record for SOMevidence 1.1.1

Validation was completed on 2026-08-11 using the release source on macOS
arm64 with R 4.5.3. This record covers local software validation. Cross-platform
continuous integration and distribution-service status are recorded separately
after the release candidate is published.

## Results

- Package metadata identify version 1.1.1, one author, and a release date that
  is not later than the current date in Asia/Shanghai.
- Version 1.1.0 function signatures and existing required object components
  remain available; the additional workflow-summary fields are additive.
- `roxygen2::roxygenise()` was run twice; the second run produced no change.
- The complete `testthat` suite passed without failures.
- Test coverage was 92.56%, above the repository's 80% release threshold.
- `lintr::lint_package()` reported no findings.
- The complete pkgdown site built successfully, including reference pages,
  articles, news, search index and validation-scope page.
- `R CMD build --compact-vignettes=both` completed successfully.
- `R CMD check --as-cran --no-manual` completed with status OK: zero errors,
  zero warnings and zero notes.
- A clean staged installation from the built source archive passed, followed by
  a workflow and registry-filter smoke test against the installed package.
- Repository and source scans found no local filesystem paths, credentials,
  access tokens, unpublished coordinates or unapproved contributor names.
- A final independent, read-only review of the complete patch reported no
  actionable defects after its findings were resolved and retested.

## Scientific and interface checks

- An over-budget workflow stops before SOM training begins.
- Large degenerate ARI and AMI cases use contingency-table equivalence checks
  and return the expected value without sample-by-sample matrices.
- Consensus summaries distinguish computational status from scientific gate
  decisions and retain completeness and coverage information.
- GUI preflight distinguishes planned, feasible and structurally ineligible SOM
  fits, identifies repeated analysis sets, and reports cross-model prerequisites
  as review information rather than scientific validation.
- GUI preflight reports the planned pairwise workload and blocks configurations
  above the workflow's comparison limit.
- Registry filters retain version 1.1.0 exact-matching behavior, including
  zero-row results for unmatched values.
- CSV parsing accepts an incomplete final line without matching localized
  warning text, while other parsing warnings and errors produce a path-safe,
  actionable message.
- The exported R script uses the same incomplete-final-line CSV handling and
  remains executable with warnings promoted to errors.
- GUI preflight validates transformation requirements even when cross-model
  references are not requested, while cross-model eligibility uses the same
  split-specific preprocessing as the fitted references.
- GUI documentation distinguishes local file handling from remote Shiny
  deployment while confirming that the package itself adds no telemetry.

External workflow runs, GitHub Pages deployment and R-universe build identity
must refer to the exact release commit; they are not inferred from this local
record.
